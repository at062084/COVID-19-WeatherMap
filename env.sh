# Setup environment to work with kubernetes cluster and the rep-* apps
export APP_ID=cwm-rshiny

# adapt to local directory structure
export BASE_DIR="/home/at062084/DataEngineering/COVID-19/COVID-19-WeatherMap"
cd $BASE_DIR

# source Cloud config
. ./ibmcloud.conf

# Specification of the k8s environment on IBM Cloud
#export IBM_LOC="eu-de"
#export IBM_ORG="REP-CF-ORG"
#export IBM_SPACE="REP-SPACE-DE"
#export IBM_RESGRP="REP-ResourceGroup"
#export IBM_K8C="REP-K8S-Cluster"
#export IBM_K8I="bp1g35gf0bfdrpfel34g"
#export IBM_K8S="cwmnamespace"
#export IBM_REG_SPACE="cwmregspace"
#export IBM_APIKEY_CLASSIC="`cat ./secrets/apikey-classic.txt`"
#export IBM_APIKEY="`cat ./secrets/apikey.txt`"
#export IBM_APP="cwm"
#export IBM_REG="de.icr.io"

# Login into IBM cloud with API key
echo "Executing: ibmcloud login -a https://cloud.ibm.com -r $IBM_LOC -g $IBM_RESGRP"
ibmcloud login -a https://cloud.ibm.com -r $IBM_LOC -g $IBM_RESGRP -apikey $IBM_APIKEY
ibmcloud config --check-version=false

CMD="ibmcloud target -o $IBM_ORG -r $IBM_LOC -s $IBM_SPACE -g $IBM_RESGRP"
echo "> Executing: $CMD"
$CMD

CMD="ibmcloud cr login"
echo "> Executing: $CMD"
$CMD

CMD="ibmcloud ks cluster config --cluster $IBM_K8I"
echo "> Executing: $CMD"
$CMD

CMD="kubectl config current-context"
echo "> Executing: $CMD"
$CMD

CMD="ibmcloud ks cluster get --cluster $IBM_K8I"
echo "> Executing: $CMD"
$CMD

# Point to cluster config file
#`ibmcloud cs cluster-config $IBM_K8C | grep export`"
#echo "> Executing: $CMD"
#$CMD

#kubectl cluster-info

# determine external IP and Port
echo ""
echo "> Listing resources in space $IBM_K8S ..."
echo "> ingresses"
kubectl describe ingresses -n $IBM_K8S | egrep -i name
echo "> services"
kubectl describe services -n $IBM_K8S| egrep -i "nodeport|name"
echo "> deployments"
kubectl describe deployments -n $IBM_K8S| grep -i name
echo "> pods"
kubectl describe pods -n $IBM_K8S | grep -i name 
echo "> nodes"
kubectl describe nodes -n $IBM_K8S| egrep -i "name|external"


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

