FROM rocker-shiny-verse:latest
# local image based on rocker/r-ver + shiny + tidyverse

RUN apt-get update -qq && apt-get -y --no-install-recommends install \
  libudunits2-dev 

# V8 required
ENV DOWNLOAD_STATIC_LIBV8=1
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
  libprotobuf-dev \
  protobuf-compiler \
  libgeos-dev \
  libnode-dev \
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
  && rm -rf /tmp/downloaded_packages
  
# RUN rm -rf /srv/shiny-server/*
WORKDIR /srv/shiny-server/COVID-19-WeatherMap
COPY ./cwm-rshiny .
RUN sudo chown -R shiny:shiny /srv/shiny-server

# Pass Kubernetes variable if available
# ENV REP_CRISPML_ENV=${REP_CRISPML_ENV}

EXPOSE 3838

CMD ["/usr/bin/shiny-server.sh"]
