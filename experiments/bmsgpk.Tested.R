library(imputeTS)
library(zoo)
# Understand Tested data from file aggregated by daily manual cron jobs

setwd("/home/at062084/DataEngineering/COVID-19/COVID-19-WeatherMap/cwm-rshiny")
source("fun.R")

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

# read bmsgpk data scraped from website. sanitize for double scrapes
bCOS_Reconstruct <- TRUE
#da <- read.csv("/home/at062084/DataEngineering/COVID-19/COVID-19-Austria/bmsgpk/data/COVID-19-austria.reconstructed.2021.csv", stringsAsFactors=FALSE) %>%
da <- readRDS("./data/COVID-19-CWM-BMSGPK-Dashboard.scraped.rda") %>%
  dplyr::mutate(Date=as.Date(Stamp)) %>%
  dplyr::arrange(Status,Date) %>%
  dplyr::select(Date, Status, AT:W) %>%
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
d_PCR <- da$V[da$Status=="Tested_PCR" & da$Date==as.Date("2021-03-26")] - da$V[da$Status=="Tested_PCR" & da$Date==as.Date("2021-03-27")]
da$V[da$Status=="Tested_PCR" & da$Date>=as.Date("2021-03-27")] <- da$V[da$Status=="Tested_PCR" & da$Date>=as.Date("2021-03-27")] + d_PCR
# Shift Tested_AG the same amount Tested_PCR was shifted, and add the same fake_PCR numbers
da$V[da$Status=="Tested_AG" & da$Date>=as.Date("2021-03-27")] <- da$V[da$Status=="Tested_AG" & da$Date>=as.Date("2021-03-27")] - d_PCR + max(fake_PCR)*2
da$V[da$Status=="Tested"] <- da$V[da$Status=="Tested_PCR"] + da$V[da$Status=="Tested_AG"]

ggplot(data=da %>% dplyr::filter(Status=="Tested_PCR"), aes(x=Date, y=V))  +
  scale_x_date(date_breaks="1 weeks", date_labels="%d.%m") +
  geom_point(color="red") + geom_line(color="red") +
  geom_point(data=da %>% dplyr::filter(Status=="Tested_AG"), color="blue") + geom_line(data=da %>% dplyr::filter(Status=="Tested_AG"), color="blue") +
  geom_point(data=da %>% dplyr::filter(Status=="Tested")) + geom_line(data=da %>% dplyr::filter(Status=="Tested")) +
  geom_point(data=da %>% dplyr::filter(Status=="Confirmed"), aes(y=10*W), color="green")


# Error correction by imputation
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

#db[db$Region=="V" & db$Status=="Tested_PCR",] <- smoothCumSumStep(db %>% dplyr::filter(Region=="V",Status=="Tested_PCR"), Date=as.Date("2021-04-07"), numDays=14) 

db[db$Region=="W" & db$Status=="Tested",] <- smoothCumSumStep(db %>% dplyr::filter(Region=="W",Status=="Tested"), Date=as.Date("2021-03-26"), numDays=35) 
db[db$Region=="W" & db$Status=="Tested_AG",] <- smoothCumSumStep(db %>% dplyr::filter(Region=="W",Status=="Tested_AG"), Date=as.Date("2021-03-27"), numDays=35) 

db <- db %>% tidyr::spread(key=Region, val=Count)

ggplot(data=db %>% dplyr::filter(Status=="Tested_PCR"), aes(x=Date, y=V))  +
  scale_x_date(date_breaks="1 weeks", date_labels="%d.%m") +
  geom_point(color="red") + geom_line(color="red") +
  geom_point(data=db %>% dplyr::filter(Status=="Tested_AG"), color="blue") + geom_line(data=db %>% dplyr::filter(Status=="Tested_AG"), color="blue") +
  geom_point(data=db %>% dplyr::filter(Status=="Tested")) + geom_line(data=db %>% dplyr::filter(Status=="Tested")) +
  geom_point(data=db %>% dplyr::filter(Status=="Confirmed"), aes(y=10*W), color="green")



# simplified imputation for data reconstructed from cron gathered html files dumped to COS
if (bCOS_Reconstruct) {
  dg <- db %>%
    tidyr::gather(key=Regions, value=Count, AT:W) %>%
    dplyr::arrange(Date, Status, Regions) %>%
    dplyr::group_by(Status, Regions) %>%
    dplyr::mutate(Count=imputeTS::na_interpolation(Count,option="linear")) %>%
    dplyr::ungroup() %>%
    dplyr::inner_join(datATRegions %>% dplyr::select(RegionID, Region, Regions, Population)) %>%
    dplyr::select(Date, RegionID, Region, Population, Status, Count, -Regions)
}



mutZle2NA <- function(x) {ifelse(x<=0,NA,x)}
mutZlt2NA <- function(x) {ifelse(x<0,NA,x)}
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

# Error correction by imputation
for (i in 1:dim(de)[1]) {
  dn$Count[dn$Date==de$Date[i] & dn$Region==de$Region[i] & dn$Status==de$Status[i]] <- NA
}

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



# Inspection plot
dq <- dm %>%  dplyr::filter(Region=="Wien")
ggplot(data=dq %>% dplyr::filter(Status=="newTested_PCR"), aes(x=Date, y=Count))  +
  scale_x_date(date_breaks="1 weeks", date_labels="%d.%m") +
  geom_point(color="red") + geom_line(color="red") +
  geom_point(data=dq %>% dplyr::filter(Status=="newTested_AG"), color="blue") + geom_line(data=dq %>% dplyr::filter(Status=="newTested_AG"), color="blue") +
  geom_point(data=dq %>% dplyr::filter(Status=="newTested")) + geom_line(data=dq %>% dplyr::filter(Status=="newTested")) +
  geom_line(data=dq %>% dplyr::filter(Status=="newConfirmed"), aes(y=100*Count), color="green")


setStatusFacts <- c("newConfirmed","curHospital","curICU","newDeaths","relConfTest","newRecovered")
setRegionSelect <- c("Burgenland","Vorarlberg","Wien")
setRegionSelect <- c("Tirol","Kärnten","Salzburg")

setStatus100k <- c("newConfirmed","curHospital","curICU","newRecovered")
setFacet <- "Region"


# plot of 2*4 graphs (exclude newRecovered)
ggplot(data=dm %>% dplyr::filter(Date>as.Date("2021-02-01"), Region %in% setRegionSelect, Status %in% setStatusFacts), aes(x=Date, y=(Count))) +
  geom_line(aes(color=Region), size=1.5) +
  facet_wrap(Status~., nrow=2, scales="free_y")+
  scale_fill_manual(values=cbPalette) +
  scale_color_manual(values=cbPalette) +
  scale_x_date(date_breaks="2 weeks", date_labels="%d.%m") +
  scale_y_continuous(limits=c(0,NA), trans="identity") +
  ggtitle("COIVD-19 Österreich: Tests (AG+PCR), Positive und Positive/Tests pro 100.000. Wochenmittel.       Daten: bmsgpk. Datenqualität: schlecht.      Bearbeitung: heuristische Korrektur/Manipulation")

# plot of 2*4 graphs (exclude newRecovered)
ggplot(data=dm %>% dplyr::filter(Date>as.Date("2021-02-01"), Region %in% setRegionSelect, Status %in% setStatus100k), aes(x=Date, y=(Count))) +
  geom_line(aes(color=Status), size=1) +
  geom_point(aes(shape=Status, color=Status), size=2) +
  facet_wrap(as.formula(paste0(setFacet,"~.")), nrow=1)+
  scale_fill_manual(values=cbPalette) +
  scale_color_manual(values=cbPalette) +
  scale_x_date(date_breaks="2 weeks", date_labels="%d.%m") +
  scale_y_continuous(limits=c(0,NA), trans="identity") +
  ggtitle("COIVD-19 Österreich: Tests (AG+PCR), Positive und Positive/Tests pro 100.000. Wochenmittel.       Daten: bmsgpk. Datenqualität: schlecht.      Bearbeitung: heuristische Korrektur/Manipulation")

ggplot(data=dm %>% dplyr::filter(Date>as.Date("2021-02-01"), Status %in% setStatus100k), aes(x=Date, y=(Count))) +
  geom_line(aes(color=Status), size=.5) +
  geom_point(aes(shape=Status, color=Status), size=1.5) +
  facet_wrap(as.formula(paste0(setFacet,"~.")), nrow=2)+
  scale_fill_manual(values=cbPalette) +
  scale_color_manual(values=cbPalette) +
  scale_x_date(date_breaks="2 weeks", date_labels="%d.%m") +
  scale_y_continuous(limits=c(1/2,NA), trans="log2", breaks=2^(-5:10)) +
  ggtitle("COIVD-19 Österreich: Positive, Spitalsbelegung, Intensivstation und Genesene pro 100.000. Wochenmittel.       Daten: bmsgpk. Datenqualität: schlecht.      Bearbeitung: heuristische Korrektur/Manipulation")


# annotate("text", x=max(dm$Date), y=2^(0:7)*1.5, label=as.character(1:8), aes(colour=Region), size=15) +

dx <-  bmsgpk.csv %>% dplyr::filter(Status=="Tested") %>% dplyr::select(Date, Count=W)
ggplot(data=smoothCumSumStep(dx, as.Date("2021-03-26")), aes(x=Date, y=Count))  +
  scale_x_date(date_breaks="1 weeks", date_labels="%d.%m") +
  geom_point()

if (bManualReconstruct) {
  # Establish grid of Date, Regions and Status
  gridDate=seq(as.Date(min(bmsgpk.csv$Stamp, na.rm=TRUE)), as.Date(max(bmsgpk.csv$Stamp, na.rm=TRUE)), by="day")
  gridRegion=colnames(bmsgpk.csv)[-c(1,2)]
  gridStatus=bmsgpk.csv$Status %>% unique()
  completeGrid <- expand.grid(Date=gridDate,Regions=gridRegion,Status=gridStatus,stringsAsFactors=FALSE)
  
  # Clean and gather
  df <- bmsgpk.csv %>% 
    dplyr::filter(!is.na(Stamp)) %>%
    dplyr::mutate(Date=as.Date(Stamp)) %>%
    dplyr::arrange(Stamp, Status) %>%
    # one entry per Status and Date
    dplyr::group_by(Date, Status) %>%
    summarize_all(max, na.rm=TRUE) %>% 
    dplyr::ungroup() %>%
    dplyr::select(-Stamp) %>%
    # Correct max() induced Inf's
    dplyr::group_by(Status) %>%
    tidyr::gather(key=Regions, value=Count, AT:W) %>%
    dplyr::mutate(Count=ifelse(Count==-Inf,NA,Count)) %>%
    dplyr::mutate(Count=ifelse(Count==Inf,NA,Count)) %>%
    dplyr::ungroup()
  
  
  # impose grid of Date, Region, Status
  dg <- completeGrid %>%
    dplyr::left_join(df) %>%
    dplyr::arrange(Date, Regions, Status) %>%
    dplyr::group_by(Regions, Status) %>%
    dplyr::mutate(Count=imputeTS::na_interpolation(Count,option="linear")) %>%
    dplyr::ungroup() %>%
    dplyr::inner_join(datATRegions %>% dplyr::select(RegionID, Region, Regions, Population)) %>%
    dplyr::select(Date, RegionID, Region, Population, Status, Count, -Regions)
}


# Dirty: Impute single NA's with mean for neighours
imputeBmsgkp1NA <- function(x) {
  idx <- which(is.na(x))
  idx <- idx[idx>305] # ignore any NA's before 2021-01-04
  x[idx] <- round((x[idx-1]+x[idx+1])/2)
  return(x)
}

bmsgpk <- data.frame(bmsgpk.csv) %>%
  # restructure
  dplyr::arrange(Date, Region) %>%
  dplyr::group_by(Region) %>%
  tidyr::spread(key=Status, value=Count, fill=NA, drop=TRUE) %>%
  dplyr::rename(sumTested=Tested, sumTestedAG=Tested_AG, sumTestedPCR=Tested_PCR, sumConfirmed=Confirmed, sumDeaths=Deaths, sumRecovered=Recovered) %>%
  dplyr::ungroup() %>%
  # impute single NA's
  dplyr::arrange(Date, Region) %>%
  dplyr::group_by(Region) %>%
  dplyr::mutate_at(vars(starts_with("sum")), imputeBmsgkp1NA) %>%
  # new*
  dplyr::mutate(newConfirmed=(sumConfirmed-lag(sumConfirmed))) %>%
  dplyr::mutate(newTested=(sumTested-lag(sumTested))) %>%
  dplyr::mutate(newTestedPCR=(sumTestedPCR-lag(sumTestedPCR))) %>%
  dplyr::mutate(newTestedAG=(sumTestedAG-lag(sumTestedAG))) %>%
  # per 100.000
  dplyr::mutate(newConfPop=newConfirmed/Population*100.000) %>%
  dplyr::mutate(newTestPop=newTested/Population*100.000) %>%
  dplyr::mutate(newTestPopPCR=newTestedPCR/Population*100.000) %>%
  dplyr::mutate(newTestPopAG=newTestedAG/Population*100.000) %>%
  # rm7*
  dplyr::mutate(rm7NewConfirmed=rollmean(newConfirmed,7, align="right", fill=NA)) %>%
  dplyr::mutate(rm7NewTested=rollmean(newTested,7, align="right", fill=NA)) %>%
  dplyr::mutate(rm7NewTestedPCR=rollmean(newTestedPCR,7, align="right", fill=NA)) %>%
  dplyr::mutate(rm7NewTestedAG=rollmean(newTestedAG,7, align="right", fill=NA)) %>%
  # per 100.000
  dplyr::mutate(rm7NewConfPop=rm7NewConfirmed/Population*100.000) %>%
  dplyr::mutate(rm7NewTestPop=rm7NewTested/Population*100.000) %>%
  dplyr::mutate(rm7NewTestPopPCR=rm7NewTestedPCR/Population*100.000) %>%
  dplyr::mutate(rm7NewTestPopAG=rm7NewTestedAG/Population*100.000) %>%
  # cur*
  dplyr::rename(curHospital=Hospitalisierung, curICU=Intensivstation) %>%
  dplyr::ungroup() %>%
  dplyr::select(Date, Region, starts_with("new"), starts_with("rm7"), starts_with("cur"), starts_with("sum"))


bm21 <- bmsgpk %>% dplyr::filter(Date>as.Date("2021-01-10")) %>%
  dplyr::filter(newConfirmed>0, newTested>0, newTestedPCR>0, newTestedAG>0)

ggplot(data=bm21, aes(x=Date)) +
  geom_line(aes(y=rm7NewTestPop), col="black") + 
  geom_line(aes(y=rm7NewTestPopPCR), col="green") + 
  geom_line(aes(y=rm7NewTestPopAG), col="blue")  +
  geom_line(aes(y=rm7NewConfPop), col="red")  +
  geom_line(aes(y=rm7NewConfPop/rm7NewTestPop), col="magenta")  +
  scale_y_continuous(trans="log10") +
  facet_wrap(Region~., nrow=2, scales="free_y") +
  ggtitle("")

bm20 <- bmsgpk %>%  dplyr::filter(Date<as.Date("2021-01-01")) %>%
  dplyr::filter(newConfirmed>0, newTested>0)

ggplot(data=bm20, aes(x=Date)) +
  geom_line(aes(y=rm7NewTestPop), col="black") + 
  geom_line(aes(y=rm7NewConfPop), col="red")  +
  geom_line(aes(y=rm7NewConfPop/rm7NewTestPop), col="magenta")  +
  scale_y_continuous(trans="log10") +
  facet_wrap(Region~., nrow=2) +
  ggtitle("")

ggplot(data=bm20, aes(x=Date)) +
  geom_line(aes(y=newTested), col="black") + 
  geom_line(aes(y=newConfirmed), col="red")  +
  geom_line(aes(y=newConfirmed/newTested), col="magenta")  +
  scale_y_continuous(trans="log10") +
  facet_wrap(Region~., nrow=2) +
  ggtitle("")

# seven day rolling means
ggplot(data=bmsgpk %>% dplyr::filter(newConfirmed>0, newTested>0), aes(x=Date)) +
  geom_line(aes(y=rm7NewTested), col="black") + 
  geom_line(aes(y=rm7NewConfirmed), col="red")  +
  geom_line(aes(y=rm7NewConfirmed/rm7NewTested), col="magenta")  +
  scale_y_continuous(trans="log10", limits=c(1e-3, 1e+5)) +
  facet_wrap(Region~., nrow=2) +
  ggtitle("BMSGPK (from COVID-19-Austria manual crons. several missing days, otherwise similar to AGES timeline data")
