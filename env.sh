# Setup environment to work with kubernetes cluster and the rep-* apps
APP_ID=cwm

# adapt to local directory structure
export BASE_DIR="/home/at062084/DataEngineering/COVID-19/COIVD-19-WeatherMap"

# Specification of the k8s environment on IBM Cloud
export IBM_LOC="eu-de"
export IBM_ORG="REP-CF-ORG"
export IBM_SPACE="REP-SPACE-DE"
export IBM_RESGRP="REP-ResourceGroup"
export IBM_K8C="REP-K8S-Cluster"
export IBM_K8S="cwmnamespace"
export IBM_REG_SPACE="cwmnamespace"
export IBM_APIKEY_CLASSIC="`cat ../secrets/apikey-class.txt`"
export IBM_APIKEY="`cat ../secrets/apikey.txt`"
export IBM_APP="cwm"

# Login into IBM cloud with API key
export IBM_REG="de.icr.io"
ibmcloud login -a https://api.$IBM_LOC.bluemix.net -apikey $IBM_APIKEY
ibmcloud config --check-version=false
ibmcloud target -o "$IBM_ORG" -r "$IBM_LOC" -s "$IBM_SPACE" -g "$IBM_RESGRP"
ibmcloud cr login

# Point to cluster config file
export CMD="`ibmcloud cs cluster-config $IBM_K8C | grep export`"
echo $CMD
$CMD

kubectl cluster-info

# determine external IP and Port
kubectl describe nodes |grep External
kubectl describe deployments | grep -i name
kubectl describe services | grep nodeport
kubectl describe ingresses 
kubectl describe pods | grep Name: | grep -i rep | grep deployment

# write scripts to login into containers on kubernetes
export REP_NODE=`kubectl describe pods | grep Name: |grep rep | grep deployment | awk '{ print $NF }'`

# Some more kubectl commands

# Cluster components
# kubectl describe endpoints
# kubectl describe ingresses
# kubectl describe services
# kubectl describe deployments
# kubectl describe replicasets
# kubectl describe nodes
# kubectl describe pods

# Configurtion
# kubectl describe configmaps
# kubectl describe secrets
# kubectl describe apiservices

# Authorization
# kubectl describe clusterroles
# kubectl describe roles
# kubectl describe rolebindings

# extract nginx config for service
#kubectl exec -ti -n kube-system -c nginx-ingress -- ls -l  /etc/nginx/
#kubectl exec -ti -n kube-system -c nginx-ingress -- ls -l  /etc/nginx/conf.d
#kubectl exec -ti -n kube-system -c nginx-ingress -- cat ./etc/nginx/default-<ingress_resource_name>.conf

# kubectl cp repnamespace/rep-influxdb-5fdfc5b48c-c5fkh:/var/lib/influxdb ./ -c rep-influxdb
# kubectl cp repnamespace/rep-admin-55696bc9fd-9sp6v:/srv ./ -c rep-admin

