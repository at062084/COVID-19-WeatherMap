#!/bin/sh
# This script tags and pushes a docker image from local registry into kubernetes registry on IBM cloud

# check arguments
if [ $# -lt 1 ]
then
	echo "usage: $0 Project_ID [dryrunflag]"
	exit 1
fi
APP_ID=$1

DRY_RUN=0
if [ $# -eq 2 ]
then
        DRY_RUN=1
        echo "Dryrun mode ..."
fi

# commands to build and deploy a docker image of itg-rshiny to kubernetes
export DKR_PRJ="itg-rshiny"
export APP_DIR="/home/at062084/DataEngineering/ITG/Kubernetes/$DKR_PRJ"
export K8S_DIR=$APP_DIR/../itg-kubernetes
export APP_CONF=$K8S_DIR/ibmcloud.$APP_ID.conf

# check for config file
if [ ! -r $APP_CONF ]
then
	echo "Error: $APP_CONF not found. Exiting"
	exit 2
fi

# Source config file
. $APP_CONF

# Extract current GIT_VER from version file
cd $APP_DIR
# GIT_VER=`cat $APP_DIR/app.R | awk '{n=index($0,"A1-ITG-"); if (n>0) {s=substr($0,n); split(s,sp," "); print substr(sp[1],8)}}'`
export GIT_VER=`cat $APP_DIR/version | tail -1 | awk '{print $NF; }'`
export DKR_TAG="$DKR_PRJ:$GIT_VER"; echo $DKR_TAG

# Login into IBM cloud with API key
export IBM_REG="registry.$IBM_LOC.bluemix.net"
ibmcloud login -a https://api.$IBM_LOC.bluemix.net -apikey $IBM_APIKEY
ibmcloud config --check-version=false
ibmcloud target -o "$IBM_ORG" -r "$IBM_LOC" -s "$IBM_SPACE" -g "$IBM_RESGRP"
#ibmcloud cs region-set eu-central
ibmcloud cr login

# tag image and upload to IBM cloud registry
CMD="docker tag  $DKR_TAG $IBM_REG/$IBM_REG_SPACE/$DKR_TAG"
echo $CMD
[ $DRY_RUN -eq 0 ] && $CMD

CMD="docker push $IBM_REG/$IBM_REG_SPACE/$DKR_TAG"
echo $CMD
[ $DRY_RUN -eq 0 ] && $CMD

ibmcloud cr images | grep $DKR_PRJ

