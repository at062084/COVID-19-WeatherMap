FROM rocker-shiny-verse
# local image based on rocker/r-ver + shiny + tidyverse

RUN  install2.r --error \
    --deps TRUE \
    leavelet \
    leaflet.extras \
  && rm -rf /tmp/downloaded_packages




