
library(forcats)


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
