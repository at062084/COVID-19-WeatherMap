options(error = function() traceback(2))


# ===========================================================================
# DataSources:
# 0.) StatA: These files have been downladd manually
# 1.) AGES: zipfile (7 csv files)
# 2.) BSMGPK: 6 csv files
# 3.) AGES: Positive nach Altersgruppe (a3) und Impfstatus
# 4.) AGES: Erkrankte nach Altersgruppe (a3) und Impfstatus
# ===========================================================================

caDataDownload <- function() {

  # Bmsgpk
  caDataDownloadBmsgpk_timelines()
  
  # Ages
  caDataDownloadAges_zipFile()
  caDataDownloadAges_impfinz()
  caDataDownloadAges_impfsym()

  return(TRUE)
}

# -------------------------------------------------------------------------------------------
# 1.) AGES: zipFile
# -------------------------------------------------------------------------------------------
caDataDownloadAges_zipFile <- function() {
  
  zipFile=paste0("./data/download/Ages/data.zip")
  unzipDir="./data/tmp"
  agesDir="./data/download/Ages"
  
  url="https://covid19-dashboard.ages.at/data/data.zip"
  cmd <- paste0(url, " -O ", zipFile)
  logMsg(paste("Executing", "wget", cmd))
  system2("wget", cmd)
  
  cmd <- paste0("-rf ./data/tmp/*")
  logMsg(paste("Executing", "rm", cmd))
  system2("rm", cmd)
  
  cmd <- paste("-o", zipFile, "-d", unzipDir)
  logMsg(paste("Executing", "unzip", cmd))
  system2("unzip",cmd)
  
  logMsg(paste("Moving files to", agesDir))
  cmd <- paste0("cd ", unzipDir, "; for i in *; do mv $i ", "../../", agesDir, "/$i; done")
  logMsg(paste("Executing", cmd))
  system(cmd)
  
  return(0)
}


# -------------------------------------------------------------------------------------------
# 2.) Bmsgpk: Downoad data from Bmsgpk
# -------------------------------------------------------------------------------------------
caDataDownloadBmsgpk_timelines <- function (bSave=TRUE) {
  
  # https://info.gesundheitsministerium.gv.at/data/timeline-eimpfpass.csv?v=2021-05-12-01-19 
  # (Datum;BundeslandID;Bevölkerung;Name;EingetrageneImpfungen;EingetrageneImpfungenPro100;Teilgeimpfte;TeilgeimpftePro100;Vollimmunisierte;VollimmunisiertePro100;
  # Gruppe_<25_M_1;Gruppe_<25_W_1;Gruppe_<25_D_1;Gruppe_25-34_M_1;Gruppe_25-34_W_1;Gruppe_25-34_D_1;Gruppe_35-44_M_1;Gruppe_35-44_W_1;Gruppe_35-44_D_1;Gruppe_45-54_M_1;Gruppe_45-54_W_1;Gruppe_45-54_D_1;Gruppe_55-64_M_1;Gruppe_55-64_W_1;Gruppe_55-64_D_1;Gruppe_65-74_M_1;Gruppe_65-74_W_1;Gruppe_65-74_D_1;Gruppe_75-84_M_1;Gruppe_75-84_W_1;Gruppe_75-84_D_1;Gruppe_>84_M_1;Gruppe_>84_W_1;Gruppe_>84_D_1;Gruppe_<25_M_2;Gruppe_<25_W_2;Gruppe_<25_D_2;Gruppe_25-34_M_2;Gruppe_25-34_W_2;Gruppe_25-34_D_2;Gruppe_35-44_M_2;Gruppe_35-44_W_2;Gruppe_35-44_D_2;Gruppe_45-54_M_2;Gruppe_45-54_W_2;Gruppe_45-54_D_2;Gruppe_55-64_M_2;Gruppe_55-64_W_2;Gruppe_55-64_D_2;Gruppe_65-74_M_2;Gruppe_65-74_W_2;Gruppe_65-74_D_2;Gruppe_75-84_M_2;Gruppe_75-84_W_2;Gruppe_75-84_D_2;Gruppe_>84_M_2;Gruppe_>84_W_2;Gruppe_>84_D_2;Gruppe_NichtZuordenbar;
  # EingetrageneImpfungenBioNTechPfizer_1;EingetrageneImpfungenModerna_1;EingetrageneImpfungenAstraZeneca_1;EingetrageneImpfungenBioNTechPfizer_2;EingetrageneImpfungenModerna_2;EingetrageneImpfungenAstraZeneca_2;EingetrageneImpfungenJanssen)
  
  # https://info.gesundheitsministerium.gv.at/data/timeline-bbg.csv
  # (Datum;BundeslandID;Bevölkerung;Name;Auslieferungen;AuslieferungenPro100;Bestellungen;BestellungenPro100)
  
  # https://info.gesundheitsministerium.gv.at/data/timeline-faelle-ems.csv?v=0.7073526087667191 
  # (Datum;BundeslandID;Name;BestaetigteFaelleEMS)
  
  # https://info.gesundheitsministerium.gv.at/data/timeline-faelle-bundeslaender.csv?v=0.818485116465528
  # (Datum;BundeslandID;Name;BestaetigteFaelleBundeslaender;Todesfaelle;Genesen;Hospitalisierung;Intensivstation;Testungen;TestungenPCR;TestungenAntigen)
  
  # https://info.gesundheitsministerium.gv.at/data/timeline-testungen-apotheken-betriebe.csv?v=0.3728616675006212
  # (Datum;BundeslandID;Name;GemeldeteTestsApotheken;GemeldeteTestsBetriebe)
  
  # https://info.gesundheitsministerium.gv.at/data/timeline-testungen-schulen.csv?v=0.7568994404841763
  # (Datum;BundeslandID;Name;GemeldeteTestsSchulen)
  
  # https://info.gesundheitsministerium.gv.at/data/faelle-international.csv?v=0.5173385309581437
  
  logMsg(paste("Downloading files from BMSGPK at https://info.gesundheitsministerium.gv.at/data/"))
  
  # List of files to download
  csvFiles <- c("timeline-eimpfpass", "timeline-bbg", "timeline-faelle-ems", "timeline-faelle-bundeslaender", 
                "timeline-testungen-apotheken-betriebe", "timeline-testungen-schulen")  
  
  # Populate new field 'Source' to identify csvFile
  csvSources <- c("tei","teb","tfe","tfb","ttab","tts")
  
  # Iterate csvFiles
  for (k in 1:length(csvFiles)) {
    csvFile <- csvFiles[k]
    #csvSource <- csvSources[k]
    url <- paste0("https://info.gesundheitsministerium.gv.at/data/", csvFile,".csv")
    logMsg(paste("Fetching", url))
    rc <- read.csv(url, header=TRUE, sep=";", stringsAsFactors=FALSE)
    
    if(bSave) {
      csvWrite <- paste0("./data/download/Bmsgpk/", csvFile, ".csv")
      logMsg(paste("Writing", csvWrite))
      write.csv(rc, csvWrite, row.names=FALSE, quote=FALSE)
    }
    
    # gather to long format
    rc <- rc %>% 
      dplyr::mutate(Datum=as.Date(Datum)) %>% 
      dplyr::select(-starts_with("Bev")) %>%
      tidyr::gather(key="Key", value="Value", -Datum, -BundeslandID, -Name) %>%
      dplyr::mutate(Source=!!csvSource)
    # logMsg(paste("  nrows:", dim(rc)[1], "firstDate", min(rc$Datum)))
    
    if (bSave) { 
      rdaFile <- paste0("./data/prepared/Bmsgpk/", csvFile, ".rda")
      logMsg(paste("Writing", rdaFile))
      saveRDS(rc, file=rdaFile)  
    }
  }
}


# -------------------------------------------------------------------------------------------
# 3.) AGES: Positive nach Altersgruppe (a3) und Impfstatus
# -------------------------------------------------------------------------------------------
caDataDownloadAges_impfinz <- function(bSave=TRUE) {
  
  # https://www.ages.at/fileadmin/AGES2015/Themen/Krankheitserreger_Dateien/Coronavirus/Inzidenz_Impfstatus/Inzidenz_Impfstatus_12bis17Jahre.csv
  # https://www.ages.at/fileadmin/AGES2015/Themen/Krankheitserreger_Dateien/Coronavirus/Inzidenz_Impfstatus/Inzidenz_Impfstatus_18bis59Jahre.csv
  # https://www.ages.at/fileadmin/AGES2015/Themen/Krankheitserreger_Dateien/Coronavirus/Inzidenz_Impfstatus/Inzidenz_Impfstatus_60plus.csv
  
  csvFiles=c("Inzidenz_Impfstatus_12bis17Jahre.csv",
             "Inzidenz_Impfstatus_18bis59Jahre.csv",
             "Inzidenz_Impfstatus_60plus.csv")
  
  # new files as of 2021-08-27
  url="https://www.ages.at/fileadmin/AGES2015/Themen/Krankheitserreger_Dateien/Coronavirus/Inzidenz_Impfstatus/"
  logMsg(paste("Downloading data from", url))
  
  urls=paste0(url, csvFiles)
  col.names=c("Date","newInzImmuYes","newInzImmuNo")
  colClasses = c("character", rep("numeric",2))
  for (i in 1:length(Urls)) {
    df <- read.csv(urls[i], header=FALSE, skip=1, sep=";", dec=",", stringsAsFactors=FALSE, col.names=col.names, colClasses=colClasses)
    if(bSave) {
      csvWrite <- paste0("./data/download/Ages/", csvFiles[i])
      logMsg(paste("Writing", csvWrite))
      write.csv(df, csvWrite, row.names=FALSE, quote=FALSE)
    }
  }
}
  
  
# -------------------------------------------------------------------------------------------
# 4.) AGES: Erkrankte nach Altersgruppe (a3) und Impfstatus
# -------------------------------------------------------------------------------------------
caDataDownloadAges_impfsym <- function(bSave=TRUE) {
  
  # https://www.ages.at/fileadmin/AGES2015/Themen/Krankheitserreger_Dateien/Coronavirus/Inzidenz_Impfstatus/Inzidenz_Impfstatus_Erkrankte_12-17_Jahre.csv
  # https://www.ages.at/fileadmin/AGES2015/Themen/Krankheitserreger_Dateien/Coronavirus/Inzidenz_Impfstatus/Inzidenz_Impfstatus_Erkrankte_18-59_Jahre.csv
  # https://www.ages.at/fileadmin/AGES2015/Themen/Krankheitserreger_Dateien/Coronavirus/Inzidenz_Impfstatus/Inzidenz_Impfstatus_Erkrankte_60__Jahre.csv
  
  csvFiles=c("Inzidenz_Impfstatus_Erkrankte_12-17_Jahre.csv",
             "Inzidenz_Impfstatus_Erkrankte_18-59_Jahre.csv",
             "Inzidenz_Impfstatus_Erkrankte_60__Jahre.csv")
  
  # new files as of 2021-08-27
  url="https://www.ages.at/fileadmin/AGES2015/Themen/Krankheitserreger_Dateien/Coronavirus/Inzidenz_Impfstatus/"
  logMsg(paste("Downloading data from", url))
  
  urls=paste0(url, csvFiles)
  
  
  col.names=c("Date","newInzSymYes","newInzSymNo")
  colClasses = c("character", rep("numeric",2))
  for (i in 1:length(Urls)) {
    df <- read.csv(Urls[i], header=FALSE, skip=1, sep=";", dec=",", stringsAsFactors=FALSE, col.names=col.names, colClasses=colClasses)
    if(bSave) {
      csvWrite <- paste0("./data/download/Ages/", csvFiles[i])
      logMsg(paste("Writing", csvWrite))
      write.csv(df, csvWrite, row.names=FALSE, quote=FALSE)
    }
  }
}

  

