#!/bin/bash

LOG="./log/cwm.github.log"
DIR=/home/shiny/github/COVID-19-WeatherMap
APP=rshiny

cd $DIR 
mkdir -p "./log" 2>/dev/null

echo "`date +%Y%m%d-%H%M%S` Running $0" | tee -a $LOG

echo "`date +%Y%m%d-%H%M%S` Fetching master/HEAD. Overwriting code changes. Preserving data" | tee -a $LOG
mkdir -p $DIR/$APP/data.org 2>/dev/null
mv $DIR/$APP/data/* $DIR/$APP/data.org

git fetch 2>&1 | tee -a $LOG
git reset --hard HEAD 2>&1 | tee -a $LOG
git merge '@{u}' 2>&1 | tee -a $LOG

mv $DIR/$APP/data.org/* $DIR/$APP/data

echo "`date +%Y%m%d-%H%M%S` Finished $0" | tee -a $LOG

