# -----------------------------------------------------------------------------------------------------
# main
# -----------------------------------------------------------------------------------------------------
library(dplyr)
library(lubridate)
library(ggplot2)

dataPath <- "./data"
agesProcessedFile <-"COVID-19-CWM-AGES-TestedProcessed.rda"
agesEvaluatedFile <-"COVID-19-CWM-AGES-TestedEvaluated.rda"
zipPath<-"../../COVID-19-Austria/bmsgpk/data/zip"
unzipPath="./data/unzip"
bUnziped <- FALSE
nSettleDays <- 7 # Number of days to wait until status 'all tests returned' reached (may or not actually be the case) 
nCalcWeeks <- 4 # Number of past weeks to use for estimation of fraction of under-reporting before nSettleDays reached


# ---------------------------------------------------
# Part 1: Generate data from past zip files
# ---------------------------------------------------
zipFiles <- list.files(path=zipPath, pattern="COVID-19-austria.V1006.2021([0-9-]*).zip")
for (zipFile in zipFiles) {

  if(!bUnziped) {
    cmd <- paste("-o", paste0(zipPath,"/",zipFile), "CovidFaelle_Timeline.csv", "-d", unzipPath)
    system2("unzip",cmd)
  }

  df <- read.csv(paste0(unzipPath,"/CovidFaelle_Timeline.csv"), stringsAsFactors=FALSE, sep=";") %>%
    dplyr::mutate(DateEvaluated=as.Date(Time, format="%d.%m.%Y %H:%M:%S")) %>%
    dplyr::mutate(RegionID=as.character(BundeslandID)) %>%
    dplyr::filter(DateEvaluated>(max(DateEvaluated)-days(nSettleDays))) %>%
    dplyr::mutate(DateReported=max(DateEvaluated)) %>%
    dplyr::select(DateReported, DateEvaluated, RegionID, Region=Bundesland, newConfirmed=AnzahlFaelle)

  if(!bUnziped) {
    cmd <- paste(paste(paste0(unzipPath,"/CovidFaelle_Timeline.csv"), paste0(unzipPath,"/CovidFaelle_Timeline-", max(df$DateReported), ".csv")))
    system2("cp",cmd)
  }
  
  if(zipFile==zipFiles[1]) {
    dp <- df
  } else {
    dp <- rbind(dp,df)
  }
}

# Data for same Evaluated/Reported day may have been read more than once. 
# Make records unique and record newConfirmed=max(newConfirmed)
dq <- dp %>% 
  dplyr::arrange(RegionID, Region, DateEvaluated, DateReported) %>%
  dplyr::group_by(RegionID, Region, DateEvaluated, DateReported) %>% 
  dplyr::summarize(newConfirmed=max(newConfirmed)) %>%
  dplyr::ungroup()
saveRDS(dq,file=paste0(dataPath,"/",agesProcessedFile))


# Add features for further calculations
do <- dq %>%
  dplyr::mutate(RegionID=as.character(RegionID)) %>%
  dplyr::mutate(WdayReported=as.character(wday(DateReported, week_start=1, label=TRUE, abbr=TRUE))) %>%
  dplyr::mutate(WdayEvaluated=as.character(wday(DateEvaluated, week_start=1, label=TRUE, abbr=TRUE))) %>%
  dplyr::mutate(MonthEvaluated=as.character(month(DateEvaluated, label=TRUE, abbr=TRUE))) %>%
  dplyr::select(DateEvaluated, MonthEvaluated, WdayEvaluated, DateReported, WdayReported, RegionID, Region, newConfirmed) %>%
  dplyr::arrange(RegionID, Region, DateEvaluated, DateReported) %>%
  dplyr::group_by(DateEvaluated, Region) %>%
  dplyr::mutate(daysDayN=as.integer(DateReported-DateEvaluated)) %>%
  dplyr::mutate(propDayN = newConfirmed/last(newConfirmed)) %>%  
  dplyr::ungroup()

str(do)
saveRDS(do,file=paste0(dataPath,"/",agesEvaluatedFile))


