# https://cloud.ibm.com/docs/containers?topic=containers-ingress-types#alb-type-migration

# -------------------------------------------------------------------------------------------
# STEP 1: TLS secrets
# -------------------------------------------------------------------------------------------
# IBM_K8S="cwmnamespace"
# IBM_K8C="REP-K8S-Cluster"

ibmcloud ks cluster get -c $IBM_K8C | grep Ingress --> OK
Ingress Subdomain:              rep-k8s-cluster-48a40589c62cd829903b0d7849def5a2-0000.eu-de.containers.appdomain.cloud   
Ingress Secret:                 rep-k8s-cluster-48a40589c62cd829903b0d7849def5a2-0000   
Ingress Status:                 healthy   
Ingress Message:                All Ingress components are healthy
SECRET="rep-k8s-cluster-48a40589c62cd829903b0d7849def5a2-0000"

ibmcloud ks ingress secret get -c $IBM_K8C --name $SECRET --namespace default --> OK
Name:           rep-k8s-cluster-48a40589c62cd829903b0d7849def5a2-0000   
Namespace:      default   
CRN:            crn:v1:bluemix:public:cloudcerts:eu-de:a/6ab957f781413f4aecccc1eacdf4ac0d:0e27be46-fcab-4201-9c93-d57dc9917b6a:certificate:8c3810be613e009b4c9d966d219459eb   
Expires On:     2021-05-17T01:53:26+0000   
Domain:         rep-k8s-cluster-48a40589c62cd829903b0d7849def5a2-0000.eu-de.containers.appdomain.cloud   
Status:         created   
User Managed:   false   
Persisted:      true   
CRN="crn:v1:bluemix:public:cloudcerts:eu-de:a/6ab957f781413f4aecccc1eacdf4ac0d:0e27be46-fcab-4201-9c93-d57dc9917b6a:certificate:8c3810be613e009b4c9d966d219459eb"

ibmcloud ks ingress secret create --cluster $IBM_K8C --cert-crn $CRN --name $SECRET --namespace $IBM_K8S --> OK
OK

ibmcloud ks ingress secret get -c $IBM_K8C --name $SECRET --namespace $IBM_K8S --> OK
Name:           rep-k8s-cluster-48a40589c62cd829903b0d7849def5a2-0000   
Namespace:      cwmnamespace   
CRN:            crn:v1:bluemix:public:cloudcerts:eu-de:a/6ab957f781413f4aecccc1eacdf4ac0d:0e27be46-fcab-4201-9c93-d57dc9917b6a:certificate:8c3810be613e009b4c9d966d219459eb   
Expires On:     2021-05-17T01:53:26+0000   
Domain:         *.rep-k8s-cluster-48a40589c62cd829903b0d7849def5a2-0000.eu-de.containers.appdomain.cloud   
Status:         created   
User Managed:   true   
Persisted:      false   

# check verison of ALB's
ibmcloud ks alb ls -c $IBM_K8C
ALB ID                                Enabled   Status     Type      ALB IP            Zone    Build                           ALB VLAN ID   NLB Version   
private-crbp1g35gf0bfdrpfel34g-alb1   false     disabled   private   -                 fra02   ingress:                        2801064       1.0   
public-crbp1g35gf0bfdrpfel34g-alb1    true      enabled    public    159.122.110.179   fra02   ingress:2452/ingress-auth:954   2801062       1.0   
VLAN_ID=2801062

# create ALB running new images
ibmcloud ks ingress alb create classic --cluster $IBM_K8C --type public --zone fra2 --vlan $VLAN_ID --version 0.35.0_869_iks

ibmcloud ks ingress alb ls -c $IBM_K8C

# -------------------------------------------------------------------------------------------
# STEP 2: Update ingress resource
# -------------------------------------------------------------------------------------------

# 1. updsate ingresss reseources
ibmcloud ks ingress alb migrate start --type test -c $IBM_K8C

# 2. Status completed
ibmcloud ks ingress alb migrate status -c $IBM_K8C

kubectl logs -n kube-system job/ingress-migration

# 3. edit resources to mitigate warnings
# Ingress
kubectl edit ingress <migrated_resource_name> -n $IBM_K8S
# Configmap
kubectl edit cm ibm-k8s-controller-config-test -n kube-system
# Test ALB service
kubectl edit deployment public-ingress-migrator -n kube-system

# 4. generate traffic 
# https://test-<ingress_subdomain>/<app1_path>

# 5. make local copies of test ingress, configmap, ALB services

# 6. clean up test resources
# Test ingress resource copies
ibmcloud ks ingress alb migrate clean -c <cluster_name_or_ID> --test-ingresses -f
# Test configmap
kubectl delete cm ibm-k8s-controller-config-test -n kube-system
# Test ALB service
kubectl delete service public-ingress-migrator -n kube-system

7. Run migration in production
ibmcloud ks ingress alb migrate start --type production -c <cluster_name_or_ID>

# 8. Check Status
ibmcloud ks ingress alb migrate status -c <cluster_name_or_ID>

# Apply any changes to test resources also to production
# Ingress
kubectl edit ingress <migrated_resource_name> -n <namespace>
# Config,ap
kubectl edit cm ibm-k8s-controller-config-test -n kube-system

# -------------------------------------------------------------------------------------------
# STEP 3: Change ALB images
# -------------------------------------------------------------------------------------------

# Create new ALBs











