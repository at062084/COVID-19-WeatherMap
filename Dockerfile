FROM rocker-shiny-verse
# local image based on rocker/r-ver + shiny + tidyverse

RUN  install2.r --error \
  --deps TRUE \
    plotly \
    leavelet \
    leaflet.extras \
    maps \
    mapproj \
  && rm -rf /tmp/downloaded_packages
  
  
# RUN rm -rf /srv/shiny-server/*
WORKDIR /srv/shiny-server/COVID-19-WeatherMap
COPY ./cwm-rshiny .
RUN sudo chown -R shiny:shiny /srv/shiny-server

# Pass Kubernetes variable if available
# ENV REP_CRISPML_ENV=${REP_CRISPML_ENV}

EXPOSE 3838

CMD ["/usr/bin/shiny-server.sh"]




