#!/bin/bash

LOG="./log/cwm.cron.log"
DIR=/home/shiny/github/COVID-19-WeatherMap
cd $DIR 
mkdir -p "./log" 2>/dev/null

echo "`date +%Y%m%d-%H%M%S` Running $0" | tee -a $LOG
Rscript ./cron.R 2>&1 | tee -a $LOG
echo "`date +%Y%m%d-%H%M%S` Finished $0" | tee -a $LOG

