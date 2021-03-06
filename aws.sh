#!/bin/bash

DIR=/home/shiny/ShinyApps/COVID-19-WeatherMap/aws

if [ $# -lt 1 ]
then
  echo "usage: $0 <command to execute [cron|github]> <list of shiny server hostnames or IPs [rshny-36|rshiny-203]>"
  exit 1
fi

CMD="cron"
if [ $# -ge 1 ]
then
  CMD=$1
fi

SHINYS="shiny-203 shiny-36"
if [ $# -ge 2 ]
then
  SHINYS="$2"
fi


for SHINY in $SHINYS
do
  echo "$0: Running $CMD.sh on host $SHINY in $DIR"
  ssh  aws-lb "ssh $SHINY sudo -u shiny $DIR/$CMD.sh"
done
