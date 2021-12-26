# Run cron.R to get current version of df
# desaster similar to bmsgpk data series

dF <- df %>%
  dplyr::filter(newConfirmed>0, newTested>0)

# raw data
ggplot(data=dF, aes(x=Date)) +
  geom_line(aes(y=newTested), col="black") + 
  geom_line(aes(y=newConfirmed), col="red")  +
  geom_line(aes(y=newConfirmed/newTested), col="magenta")  +
  scale_y_continuous(trans="log10") +
  facet_wrap(Region~., nrow=2) +
  ggtitle("")

# seven day rolling means
ggplot(data=dF, aes(x=Date)) +
  geom_line(aes(y=rm7NewTested), col="black") + 
  geom_line(aes(y=rm7NewConfirmed), col="red")  +
  geom_line(aes(y=rm7NewConfirmed/rm7NewTested), col="magenta")  +
  scale_y_continuous(trans="log10", limits=c(1e-3, 1e+5)) +
  facet_wrap(Region~., nrow=2) +
  ggtitle("AGES (from cron.R and subsequent processing for WeatherMap)")
