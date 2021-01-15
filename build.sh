#!/bin/sh

# This script commits and tags the itg-rshiny application into the git repository in bluemix
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

# commands to build and deploy a docker image of a1-itg to kubernetes
export GIT_VER="$1"
export GIT_MSG="$2"

# commands to build and deploy a docker image of a1-itg to kubernetes
export GIT_UID="thomas.strehl"
export GIT_USR="thomas.strehl@at.ibm.com"
export GIT_REPO="git@git.eu-de.bluemix.net:$GIT_UID"
export GIT_PRJ="itg-dashboard"
export GIT_TAG="$GIT_PRJ-$GIT_VER"

export APP_DIR="/home/at062084/DataEngineering/ITG/Kubernetes/$GIT_PRJ"
cd $APP_DIR

# Patch Git Tag into source code to display in dashboard
APP_VER=`cat $APP_DIR/src/dashboard/app.R | awk '{if(index($0,"A1-ITG-")){split($0,sp,"\"");id=sp[2];gsub("@","\\\@",id);print id}}'`
NEW_VER="A1-ITG-$GIT_VER IBM\@`date +%Y%m%d`"
CMD="perl -pi -e \"s/$APP_VER/$NEW_VER/\" $APP_DIR/src/dashboard/app.R"
echo $CMD
[ $DRY_RUN -eq 0 ] && $CMD

# Commit, tag and push current source code to git repo
[ $DRY_RUN -eq 0 ] &&  echo "`date +%Y%m%d%H%M%S` $GIT_PRJ $GIT_VER" | tee -a ./version
echo "git tag -a $GIT_TAG -m $GIT_MSG"

[ $DRY_RUN -eq 0 ] && git add version Dockerfile* *.sh *.md src/*
[ $DRY_RUN -eq 0 ] && git commit -m "$GIT_MSG"
[ $DRY_RUN -eq 0 ] && git tag -a $GIT_TAG -m "$GIT_MSG"
[ $DRY_RUN -eq 0 ] && git push origin
[ $DRY_RUN -eq 0 ] && git push origin $GIT_TAG

# Docker build file
export DKR_PRJ=$GIT_PRJ
export DKR_BLD="Dockerfile"
export DKR_TAG="$DKR_PRJ:$GIT_VER"
export DKR_SHINY_PORT=3838
export DKR_NGINX_PORT=3880
CMD="docker build -f $DKR_BLD -t $DKR_TAG ."
echo $CMD
[ $DRY_RUN -eq 0 ] && $CMD

# run image locally
CMD="docker run -d -p $DKR_NGINX_PORT:$DKR_NGINX_PORT -p $DKR_SHINY_PORT:$DKR_SHINY_PORT --name $DKR_PRJ $DKR_TAG"
echo $CMD

# Login to container
CMD="docker exec -it $DKR_PRJ bash"
echo $CMD

