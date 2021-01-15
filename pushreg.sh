#!/bin/sh
# This script tags and pushes a docker image from local registry into kubernetes registry on IBM cloud

DRY_RUN=0
if [ $# -eq 1 ]
then
        DRY_RUN=1
        echo "Dryrun mode ..."
fi

# source Cloud config
. ./ibmcloud.conf

# Constants
# Name of directory that containes rshiny files
export BASE_DIR="/home/at062084/DataEngineering/COVID-19"
export GIT_PRJ="COVID-19-WeatherMap"
export DKR_PRJ=cwm-rshiny
export APP_DIR=$BASE_DIR/$GIT_PRJ
export PRJ_DIR="$BASE_DIR/$GIT_PRJ"
cd $APP_DIR

# Extract latest GIT_VER from version file
# GIT_VER=`cat $APP_DIR/app.R | awk '{n=index($0,"A1-ITG-"); if (n>0) {s=substr($0,n); split(s,sp," "); print substr(sp[1],8)}}'`
export GIT_VER=`cat $APP_DIR/version | tail -1 | awk '{print $NF; }'`
export DKR_TAG="$DKR_PRJ:$GIT_VER"; echo $DKR_TAG

# commands to build and deploy a docker image of itg-rshiny to kubernetes

# tag image and upload to IBM cloud registry
CMD="docker tag  $DKR_TAG $IBM_REG/$IBM_REG_SPACE/$DKR_TAG"
echo "> Executing $CMD"
[ $DRY_RUN -eq 0 ] && $CMD

CMD="> docker push $IBM_REG/$IBM_REG_SPACE/$DKR_TAG"
echo "Executing $CMD"
[ $DRY_RUN -eq 0 ] && $CMD

echo "> Checking for $DKR_PRJ images"
ibmcloud cr images | grep $DKR_PRJ

