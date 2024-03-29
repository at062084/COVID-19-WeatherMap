library(ggplot2)
library(dplyr)
library(tidyr)

df <- read.csv("AntiBody.csv")
df$Datum <- as.POSIXct(as.character(df$Datum))
df$ID <- factor(df$ID)
str(df)
summary(df)

# Exctract date for 'ZweiteImpfung
di <- df %>% dplyr::filter(ImpfStatus=="ZweiteImpfung", Aktion=="Impfung") %>%
  dplyr::group_by(ID) %>%
  dplyr::mutate(DatumZweiteImpfung=Datum) %>%
  dplyr::select(ID, DatumZweiteImpfung)

# Calculate new TimeScale based on DatumZwiteImpfung 
df <- df %>%
  dplyr::left_join(di) %>%
  dplyr::group_by(ID) %>%
  dplyr::mutate(Monat = as.numeric((Datum-DatumZweiteImpfung)/3600/24/30)) %>%
  dplyr::ungroup()

# Regression Models
Monat=seq(-4,12,by=.1)

ds1 <- df %>% dplyr::filter(ImpfStoff=="BiontecPfizer", ImpfStatus=="ZweiteImpfung", Aktion=="AntiKörper")
lm1 <- lm(formula=log2(Wert)~Monat, data=ds1)
pred1 <- 2^predict(lm1, newdata=data.frame(Monat=Monat))
dp1 <- data.frame(Monat=Monat, Wert=pred1, ImpfStoff="BiontecPfizer")
summary(lm1)
(HalbwertsZeit = -1/coef(lm1)[2])

ds2 <- df %>% dplyr::filter(ImpfStoff=="AstraZenecca", ImpfStatus=="ZweiteImpfung", Aktion=="AntiKörper")
lm2 <- lm(formula=log2(Wert)~Monat, data=ds2)
pred2 <- 2^predict(lm2, newdata=data.frame(Monat=Monat))
dp2 <- data.frame(Monat=Monat, Wert=pred2, ImpfStoff="AstraZenecca")
summary(lm2)
(HalbwertsZeit = -1/coef(lm2)[2])


ggplot(data=df %>% dplyr::filter(Aktion=="AntiKörper"), aes(x=Monat, y=Wert, shape=ImpfStatus, color=ImpfStatus, group=ImpfStatus)) +
  scale_x_continuous(limit=c(-3.1,9), breaks=-3:9, sec.axis=dup_axis()) +
  scale_y_continuous(limit=c(-50,2000), breaks=seq(0,1900,by=100), sec.axis=dup_axis(), expand=c(0.0,0.0)) +
  geom_vline(data=df %>% dplyr::filter(Aktion=="Impfung"), aes(xintercept=Monat, color=ImpfStatus), size=2) +
  geom_point(size=3, color="blue") + 
  geom_line() +
  facet_grid(ImpfStoff~.) +
  geom_line(data=dp1, aes(x=Monat, y=Wert), inherit.aes=FALSE, linetype=3) +
  geom_line(data=dp2, aes(x=Monat, y=Wert), inherit.aes=FALSE, linetype=3) +
  ggtitle("Vaccination and AntiBody Test results: 2 Samples (AgeGroup 55-65, M and W), Exponential Model, BAU units")


ggplot(data=df %>% dplyr::filter(Aktion=="AntiKörper"), aes(x=Monat, y=Wert, shape=ImpfStatus, color=ImpfStatus, group=ImpfStatus)) +
  scale_x_continuous(limit=c(-3.1,9), breaks=-3:9, sec.axis=dup_axis()) +
  scale_y_continuous(limit=c(10,2000), breaks=2^seq(0:12), sec.axis=dup_axis(), trans="log2") +
  geom_vline(data=df %>% dplyr::filter(Aktion=="Impfung"), aes(xintercept=Monat, color=ImpfStatus), size=2) +
  geom_point(size=3, color="black") + 
  geom_line() +
  facet_grid(ImpfStoff~.) +
  geom_line(data=dp1, aes(x=Monat, y=Wert), inherit.aes=FALSE, linetype=3) +
  geom_line(data=dp2, aes(x=Monat, y=Wert), inherit.aes=FALSE, linetype=3) +
  ggtitle("Vaccination and AntiBody Test results: 2 Samples (AgeGroup 55-65, M and W), Exponential Model, logScale y, BAU units for half/double")

