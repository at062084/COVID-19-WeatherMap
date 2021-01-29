FROM rocker-shiny-verse:latest
# local image based on rocker/r-ver + shiny + tidyverse

# V8 required
ENV DOWNLOAD_STATIC_LIBV8=1
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
  libudunits2-dev \
  protobuf-compiler \
  libprotobuf-dev \
  libgeos-dev \
  libnode-dev \
  libgdal-dev \
  libjq-dev

RUN  install2.r --error \
    plotly \
    leaflet \
    leaflet.extras \
    RColorBrewer \
  && rm -rf /tmp/downloaded_packages
  
RUN  install2.r --error \
    geojsonsf \
    spdplyr \
  && rm -rf /tmp/downloaded_packages

RUN  install2.r --error \
    geojsonio \
    DT \
  && rm -rf /tmp/downloaded_packages
  
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
  cron

RUN  install2.r --error \
    cronR \
    zoo \
    tibbletime \
    scales \
    forcats \
  && rm -rf /tmp/downloaded_packages
 
RUN  install2.r --error \
    httr \
  && rm -rf /tmp/downloaded_packages
 
RUN rm -rf /srv/shiny-server/*
WORKDIR /srv/shiny-server/COVID-19-WeatherMap/secrets
COPY ./secrets . 
WORKDIR /srv/shiny-server/COVID-19-WeatherMap
COPY ./cwm-rshiny .
RUN rm /srv/shiny-server/COVID-19-WeatherMap/log/*

RUN sudo chown -R shiny:shiny /srv/shiny-server \
    && touch /etc/cron.allow \
    && echo shiny >> /etc/cron.allow \
    && perl -p -i -e s/101:/101:shiny/g /etc/group \
    && perl -p -i -e s/:27:/:27:shiny/g /etc/group \
    && echo "shiny ALL=NOPASSWD: /usr/sbin/cron" >>/etc/sudoers \
    && echo "shiny ALL=NOPASSWD: /usr/sbin/service" >>/etc/sudoers

# Pass Kubernetes variable if available
ENV APPLICATION_LOGS_TO_STDOUT=${APPLICATION_LOGS_TO_STDOUT}

EXPOSE 3838

CMD ["/usr/bin/shiny-server.sh"]
