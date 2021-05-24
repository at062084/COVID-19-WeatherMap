
library(forcats)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)

source("fun.R")

df <- read.csv("./data/CovidFaelle_Altersgruppe.csv", sep=";", stringsAsFactors=FALSE)
str(df)
levels(factor(df$Altersgruppe))
df$Altersgruppe <- fct_reorder(factor(df$Altersgruppe),df$AltersgruppeID,sum)
levels(df$Altersgruppe)

dg <- df %>% dplyr::left_join(datATRegions %>% dplyr::select(Region,Population), by=c("Bundesland"="Region"))
str(dg)

dh <- dg %>% 
  dplyr::arrange(Bundesland,Altersgruppe) %>%
  dplyr::group_by(Bundesland) %>%
  dplyr::mutate(csAnzEinwohner = cumsum(AnzEinwohner)) %>%
  dplyr::mutate(csAnzahl = cumsum(Anzahl)) %>%
  dplyr::mutate(csAnzahlTot = cumsum(AnzahlTot)) %>%
  dplyr::mutate(proDeathCumSum = cumsum(AnzahlTot/sum(AnzahlTot))) %>%
  dplyr::mutate(proAnzahlCumSum = cumsum(Anzahl/sum(Anzahl))) %>%
  dplyr::ungroup()

ggplot(dh, aes(x=Altersgruppe, y=AnzahlTot/Anzahl, fill=Geschlecht, color=Geschlecht)) + 
  geom_col(position="dodge") +
  geom_point(aes(y=proAnzahlCumSum)) +
  geom_point(aes(y=proDeathCumSum)) +
  facet_wrap(Bundesland~., nrow=5)


ggplot(dh, aes(x=Altersgruppe, y=proBundeslandAltersgruppe, fill=Geschlecht, color=Geschlecht)) + 
  geom_col(position="dodge") +
  #geom_point(aes(x=Altersgruppe, y=proDeathCumSum, color=Geschlecht)) +
  facet_wrap(Bundesland~., nrow=5)


df.202103 <- read.csv("./data/CovidFaelle_Altersgruppe.20210331.csv", sep=";", stringsAsFactors=FALSE)
df.202103$Altersgruppe <- fct_reorder(factor(df.202103$Altersgruppe),df.202103$AltersgruppeID,sum)
df.202103 <- df.202103 %>% 
  dplyr::mutate(SnapShot="202103") %>% 
  dplyr::select(-AltersgruppeID, -BundeslandID, -AnzahlGeheilt) %>%
  dplyr::arrange(Bundesland,Altersgruppe,AnzEinwohner)

df.202012 <- read.csv("./data/CovidFaelle_Altersgruppe.20201231.csv", sep=";", stringsAsFactors=FALSE)
df.202012$Altersgruppe <- fct_reorder(factor(df.202012$Altersgruppe),df.202012$AltersgruppeID,sum)
df.202012 <- df.202012 %>% 
  dplyr::mutate(SnapShot="202012") %>% 
  dplyr::select(-AltersgruppeID, -BundeslandID, -AnzahlGeheilt) %>%
  dplyr::arrange(Bundesland,Altersgruppe,AnzEinwohner)

df.202010 <- read.csv("./data/CovidFaelle_Altersgruppe.20201006.csv", sep=";", stringsAsFactors=FALSE)
df.202010$Altersgruppe <- fct_reorder(factor(df.202010$Altersgruppe),df.202010$AltersgruppeID,sum)
df.202010 <- df.202010 %>% 
  dplyr::mutate(SnapShot="202010") %>% 
  dplyr::select(-AltersgruppeID, -BundeslandID, -AnzahlGeheilt) %>%
  dplyr::arrange(Bundesland,Altersgruppe,AnzEinwohner)

df.202012[,c("Anzahl", "AnzahlTot")] <- df.202012[,c("Anzahl", "AnzahlTot")] - df.202010[,c("Anzahl", "AnzahlTot")]
df.202103[,c("Anzahl", "AnzahlTot")] <- df.202103[,c("Anzahl", "AnzahlTot")] - df.202012[,c("Anzahl", "AnzahlTot")]

# Put three datasets into long dataframe
df <- rbind(df.202103, df.202012, df.202010)

# Calculate mean number of AnzahlEinwohner over SnapShots
da <- df %>% dplyr::group_by(Bundesland, Altersgruppe, Geschlecht) %>%
  dplyr::summarize(AnzEinwohner=round(mean(AnzEinwohner)))

# Extract data for Österreich
do <- df %>% dplyr::select(-AnzEinwohner) %>%
  dplyr::inner_join(da) %>%
  dplyr::filter(Bundesland=="Österreich") %>%
  dplyr::mutate(AltersGruppe=as.integer(Altersgruppe)) %>%
  # Calc per 100.000
  dplyr::mutate(Anzahl100k = Anzahl/AnzEinwohner*100000) %>%
  dplyr::mutate(AnzahlTot100k = AnzahlTot/AnzEinwohner*100000) %>%
  # Calc 
  dplyr::group_by(SnapShot) %>%
  dplyr::mutate(Anzahl100kProp = Anzahl100k/sum(Anzahl100k)) %>%
  dplyr::mutate(AnzahlTot100kProp = AnzahlTot100k/sum(AnzahlTot100k)) %>%
  dplyr::ungroup()

  dp <- do  %>% dplyr::filter(SnapShot=="202103") 

  ggElementsLin <- list(
      geom_line(),
      geom_point(size=3),
      scale_x_continuous(breaks=1:10, labels=levels(dp$Altersgruppe)),
      scale_y_continuous(limit=c(0,NA))
  )
  ggElementsLog <- list(
    geom_line(),
    geom_point(size=3),
    scale_x_continuous(breaks=1:10, labels=levels(dp$Altersgruppe)),
    scale_y_continuous(limit=c(.1,NA), trans="log10")
  )
  
# Anzahl Einwohner, Positive, Sterbefälle
gg1 <- ggplot(data=dp, aes(x=AltersGruppe, y=AnzEinwohner, shape=Geschlecht, color=Geschlecht, fill=Geschlecht)) +
  ggElementsLin +
  ggtitle("Österreich: Anzahl Einwohner je Altersgruppe")

gg2 <- ggplot(data=dp, aes(x=AltersGruppe, y=Anzahl, shape=Geschlecht, color=Geschlecht, fill=Geschlecht)) +
  ggElementsLin +
  ggtitle("Österreich: Anzahl Positive je Altersgruppe")

gg3 <- ggplot(data=dp, aes(x=AltersGruppe, y=AnzahlTot, shape=Geschlecht, color=Geschlecht, fill=Geschlecht)) +
  ggElementsLin +
  ggtitle("Österreich: Anzahl Sterbefälle je Altersgruppe")


# Anteil Einwohner, Positive, Sterbefälle
gg4 <- ggplot(data=dp, aes(x=AltersGruppe, y=Anzahl/AnzEinwohner, shape=Geschlecht, color=Geschlecht, fill=Geschlecht)) +
  ggElementsLin +
  ggtitle("Anteil Positive an Bevölkerung nach Altersgruppe")

gg5 <- ggplot(data=dp, aes(x=AltersGruppe, y=AnzahlTot/AnzEinwohner, shape=Geschlecht, color=Geschlecht, fill=Geschlecht)) +
  ggElementsLin +
  ggtitle("Anteil Sterbefälle an Bevölkerung nach Altersgruppe")

gg6 <- ggplot(data=dp, aes(x=AltersGruppe, y=AnzahlTot/Anzahl, shape=Geschlecht, color=Geschlecht, fill=Geschlecht)) +
  ggElementsLin +
  ggtitle("Anteil Sterbefälle an Positiven nach Altersgruppe")



grid.arrange(gg1, gg2,  gg4,  gg3, gg5, gg6, ncol=3)



ggplot(data=do %>% dplyr::filter(SnapShot=="202103", AnzahlTot>0), aes(x=AltersgruppeID, y=AnzahlTot/Anzahl*100000, shape=Geschlecht, color=Geschlecht, fill=Geschlecht)) +
  #geom_col(position="dodge") +
  # geom_path() +
  geom_line() +
  geom_point(size=3) +
  scale_x_continuous(limits=c(1,10), breaks=1:10, labels=levels(do$Altersgruppe)) +
  scale_y_continuous(limit=c(1,NA), trans="log10") +
  ggtitle("Anteil Verstorbene je Altersgruppe")


dg <- df %>% dplyr::select(-AnzEinwohner) %>%
  dplyr::inner_join(da) %>%
  # summarize Geschlecht
  dplyr::select(-Geschlecht) %>%
  dplyr::group_by(Bundesland, Altersgruppe, SnapShot) %>%
  dplyr::summarize_all(sum) %>%
  dplyr::ungroup() %>%
  # Calc per 100.000
  dplyr::mutate(Anzahl100k = Anzahl/AnzEinwohner*100000) %>%
  dplyr::mutate(AnzahlTot100k = AnzahlTot/AnzEinwohner*100000) %>%
  # Calc 
  dplyr::group_by(Bundesland, SnapShot) %>%
  dplyr::mutate(Anzahl100kProp = Anzahl100k/sum(Anzahl100k)) %>%
  dplyr::mutate(AnzahlTot100kProp = AnzahlTot100k/sum(AnzahlTot100k)) %>%
  dplyr::ungroup()
  
#, color=Geschlecht, fill=Geschlecht
ggplot(data=dg %>% dplyr::filter(AnzahlTot100k>1), aes(x=log10(AnzahlTot100k), y=Altersgruppe)) +
  geom_col(position="dodge") + 
  facet_grid(Bundesland~SnapShot)

ggplot(data=dg %>% dplyr::filter(Anzahl100k>1), aes(y=log2(Anzahl100k), x=as.integer(Altersgruppe), color=SnapShot)) +
  scale_y_continuous(breaks=seq(0,20,by=2)) +
  geom_line() + 
  geom_point() + 
  facet_grid(Bundesland~Geschlecht)

ggplot(data=dg %>% dplyr::filter(AnzahlTot100kProp>0), 
       aes(y=AnzahlTot100kProp, x=as.integer(Altersgruppe), color=SnapShot, shape=SnapShot)) +
  scale_y_continuous(trans="log10") +
  scale_x_continuous(limits=c(1,10), breaks=1:10, labels=levels(factor(df$Altersgruppe))) +
  geom_line() + 
  geom_point() + 
  facet_wrap(Bundesland~., nrow=2)

