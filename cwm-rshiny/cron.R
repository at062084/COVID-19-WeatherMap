#!/usr/lib64/R/bin/Rscript

options(error = function() traceback(2))
#setwd("/srv/shiny-server/COVID-19-WeatherMap")
setwd("/home/at062084/DataEngineering/COVID-19/COVID-19-WeatherMap/cwm-rshiny")

logDir = "./log"
logFile <- "cwm.cron.log"
logMsg <- function(msg, sessionID="__cron__") {
  cat(paste(format(Sys.time(), "%Y%m%d-%H%M%OS3"), sessionID, msg, "\n"), file=paste0(logDir,"/",logFile), append=TRUE)
  cat(paste(format(Sys.time(), "%Y%m%d-%H%M%OS3"), sessionID, msg, "\n"))
}

# Download AGES Data Files from AGES website at www.data.gv.at/covid-19
logMsg(paste("cron: Start Running script cron.R in ", getwd()),"__cron__")
logMsg("Sourcing fun.R","__cron__")
source("./fun.R")
logMsg("Sourcing ages.R","__cron__")
source("./ages.R")

# Bundesländer
logMsg("DownLoading CovidFallzahlen data from AGES ...","__cron__")
dt <- caAgesRead_cfz()
logMsg("Downloading CovidFaelle_Timeline data from AGES ...","__cron__")
dc <- caAgesRead_cftl()

# Bezirke 
logMsg("Downloading CovidFaelle_Timeline_GKZ data from AGES ...","__cron__")
db <- caAgesRead_cfGKZtl()


# Construct working data frame 
logMsg("Start Joining CovidFaelle_Timeline with CovidFallzahlen and creating new features ...","__cron__")
df <- caAgesRead_tlrm(cftlFile="./data/CovidFaelle_Timeline.rda", cfzFile="./data/CovidFallzahlen.rda", bPlot=FALSE, 
                      nRm7Days=7, bDt7=TRUE, nDt7Days=7, bLpr=TRUE, nLprDays=19,
                      bResiduals=TRUE, dResFirst=as.Date("2020-07-01"), dResLast=as.Date("2020-12-07"), bShiftDown=TRUE,
                      bPredict=TRUE, nPolyDays=7, nPoly=2,
                      bEstimate=FALSE, bCompleteCases=FALSE)
logMsg("Done Joining CovidFaelle_Timeline with CovidFallzahlen and creating new features","__cron__")

logMsg(paste("cron: Done Running script cron.R in ", getwd()),"__cron__")



