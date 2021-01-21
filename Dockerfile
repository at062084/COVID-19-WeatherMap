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
  
# RUN rm -rf /srv/shiny-server/*
WORKDIR /srv/shiny-server/COVID-19-WeatherMap
COPY ./cwm-rshiny .
RUN sudo chown -R shiny:shiny /srv/shiny-server

# Pass Kubernetes variable if available
# ENV REP_CRISPML_ENV=${REP_CRISPML_ENV}

EXPOSE 3838

CMD ["/usr/bin/shiny-server.sh"]
