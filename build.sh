#!/bin/sh

# This script commits and tags the cwm-rshiny application into the git repository
if [ $# -lt 2 ]
then
	echo "usage: $0 version commitmsg dryrunflag (e.g. 1.0.0, \"Mofify something\" true)"
	exit 1
fi

DRY_RUN=0
if [ $# -eq 3 ]
then
	DRY_RUN=1
	echo "Dryrun mode ..."
fi

# Parameters
# commands to build and deploy a docker image of cwm-rshiny to kubernetes
export GIT_VER="$1"
export GIT_MSG="$2"

# Constants
# Name of directory that containes rshiny files
export BASE_DIR="/home/at062084/DataEngineering/COVID-19"
export GIT_PRJ="COVID-19-WeatherMap"
export DKR_PRJ=cwm-rshiny
export APP_DIR=$BASE_DIR/$GIT_PRJ

# commands to build and deploy a docker image of a1-itg to kubernetes
export GIT_UID="at062084"
export GIT_USR="at062084"
export GIT_REPO="git@github.com:$GIT_USR/$GIT_PRJ.git"
export GIT_TAG="$GIT_PRJ-$GIT_VER"

export PRJ_DIR="$BASE_DIR/$GIT_PRJ"
env | egrep "GIT|IBM|DIR" | sort

cd $PRJ_DIR

# Patch Git Tag into source code to display in dashboard
APP_VER=`cat $APP_DIR/$DKR_PRJ/app.R | awk '{if(index($0,"CWM-V")){split($0,sp,"\"");print sp[2]}}'`
NEW_VER="CWM-V$GIT_VER@`date +%Y-%m-%d`"
echo "Current: $APP_VER  New Version: $NEW_VER"
perl -pi -e "s/$APP_VER/$NEW_VER/" $APP_DIR/$DKR_PRJ/app.R

# Commit, tag and push current source code to git repo
[ $DRY_RUN -eq 0 ] &&  echo "`date +%Y%m%d%H%M%S` $GIT_PRJ $GIT_VER" | tee -a ./version
echo "git tag -a $GIT_TAG -m $GIT_MSG"

[ $DRY_RUN -eq 0 ] && git add version Dockerfile* *.sh *.yaml install.log $DKR_PRJ/*
[ $DRY_RUN -eq 0 ] && git commit -m "$GIT_MSG"
[ $DRY_RUN -eq 0 ] && git tag -a $GIT_TAG -m "$GIT_MSG"
[ $DRY_RUN -eq 0 ] && git push -u origin master
[ $DRY_RUN -eq 0 ] && git push -u origin master $GIT_TAG

# Docker build file
export DKR_BLD="Dockerfile"
export DKR_TAG="$DKR_PRJ:$GIT_VER"
export DKR_SHINY_PORT=3838
CMD="docker build -f $DKR_BLD -t $DKR_TAG ."
echo $CMD
[ $DRY_RUN -eq 0 ] && $CMD

# run image locally
CMD="docker run -d -p $DKR_SHINY_PORT:$DKR_SHINY_PORT --name $DKR_PRJ $DKR_TAG"
echo $CMD

# Login to container
CMD="docker exec -it $DKR_PRJ bash"
echo $CMD

