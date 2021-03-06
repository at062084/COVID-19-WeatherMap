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
  url <- as.character(read.csv("./secrets/slack.txt",header=FALSE)[1,1])
  body <- list(text = paste(paste0(now()," *",title,"*: "), paste0(hostName,": ",msg)))
  r <- POST(url, content_type_json(), body = body, encode = "json")
  invisible(r)
}
slackMsg(title="COVID-19-WeatherMap",msg=paste("Start cron job cron.R"))

logMsg(paste("cron: Start Running script cron.R in ", getwd()),"__cron__")
logMsg("Sourcing fun.R","__cron__")
source("./fun.R")
logMsg("Sourcing ages.R","__cron__")
source("./ages.R")
logMsg("Sourcing bmsgpk.R","__cron__")
source("./bmsgpk.R")



# -----------------------------------------------------------------------------------
# DownLoad data from BMSGPK
# -----------------------------------------------------------------------------------

# Confirmed+Tested BundesLänder 
logMsg("Downloading timeline-faelle-bundeslaender data from BMSGPK ...","__cron__")
dd <- caBmsgpkRead_tfb()

# Confirmed EMS 
logMsg("Downloading timeline-faelle-ems data from BMSGPK ...","__cron__")
de <- caBmsgpkRead_tfe()

# bmsgpk data download
logMsg("Downloading data from BMSGPK Website ...","__cron__")
bd <- caBmsgpkDownLoad ()

# bmsgpk Webpage scraper
logMsg("Scraping BMSGPK Website ...","__cron__")
#dm <- caBmsgpkUpdateDashboard()


# -----------------------------------------------------------------------------------
# Download data from AGES
# -----------------------------------------------------------------------------------

# Bundesländer
logMsg("DownLoading CovidFallzahlen data from AGES ...","__cron__")
dt <- caAgesRead_cfz()

logMsg("Downloading CovidFaelle_Timeline data from AGES ...","__cron__")
dc <- caAgesRead_cftl()

logMsg("Creating history of AGES reports of Confirmed cases ...", "__cron__")
dq <- caAgesConfHistory(dc %>% 
                          dplyr::select(Date, RegionID, Region, newConfirmed) %>% 
                          dplyr::filter(Date> max(Date)-days(nSettleDays)))

# Bezirke 
logMsg("Downloading CovidFaelle_Timeline_GKZ data from AGES ...","__cron__")
db <- caAgesRead_cfGKZtl()

#logMsg("Scraping Mutations data from AGES ...","__cron__")
# dm <-caAgesRead_Mutations()

# Construct working data frame 
logMsg("Start Joining CovidFaelle_Timeline with CovidFallzahlen and creating new features ...","__cron__")
df <- caAgesRead_tlrm(cftlFile="./data/CovidFaelle_Timeline.rda", cfzFile="./data/CovidFallzahlen.rda", bPlot=FALSE, 
                      nRm7Days=7, bDt7=TRUE, nDt7Days=7, bLpr=TRUE, nLprDays=19,
                      bResiduals=TRUE, dResFirst=as.Date("2020-07-27"), dResLast=as.Date("2020-11-16"), bShiftDown=TRUE,
                      bPredict=TRUE, nPolyDays=7, nPoly=2,
                      bEstimate=FALSE, bCompleteCases=FALSE)
logMsg("Done Joining CovidFaelle_Timeline with CovidFallzahlen and creating new features","__cron__")

slackMsg(title="COVID-19-WeatherMap",msg=paste("Complete cron job cron.R"))
logMsg(paste("cron: Done Running script cron.R in ", getwd()),"__cron__")

