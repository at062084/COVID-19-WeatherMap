#!/bin/bash

LOG="./log/cwm.github.log"
DIR=/home/shiny/github/COVID-19-WeatherMap
cd $DIR 
mkdir -p "./log" 2>/dev/null

echo "`date +%Y%m%d-%H%M%S` Running $0" | tee -a $LOG
git pull 2>&1 | tee -a ./log/cwm.bitbucket.log
echo "`date +%Y%m%d-%H%M%S` Finished $0" | tee -a $LOG