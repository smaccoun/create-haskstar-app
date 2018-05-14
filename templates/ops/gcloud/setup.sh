gcloud container clusters create "${APPNAME}-cluster" --num-nodes=2
gcloud instance create "${APPNAME}" 

k8 apply -f ops/kubernetes/tiller-service-account.yaml
helm init --service-account tiller --upgrade
helm install --name nginx-ingress stable/nginx-ingress --set rbac.create=true

helm install \
    --name cert-manager \
    --namespace kube-system \
    stable/cert-manager