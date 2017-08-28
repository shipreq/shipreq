Overview
========

There are diagrams in `../Design` that show all the infrastructure involved
and their relationships.


Setup
=====

## GCP account setup

Create account.
Create Projects from UI:
* shipreq-dev
* shipreq-prod

## Local setup

yaourt -S --needed --noconfirm aur/google-cloud-sdk
sudo pacman -S --needed pwgen terraform

sudo gcloud components install kubectl
sudo gcloud components install beta
sudo gcloud components install alpha
gcloud auth login


Initial Deployment
==================

## Overview

1. Source code & artifacts
2. Target environment
3. Provision infrastructure
4. Configure cluster
5. Deploy

## 1. Source code & artifacts

* From the GUI
  * Set project to shipreq-dev
  * Goto Source Repositories → Create
  * Create repo called shipreq
* git config credential.helper gcloud.sh
* git remote add google https://source.developers.google.com/p/shipreq-dev/r/shipreq

TODO
* work out docker versioning scheme
* automate local tag & push
* build docker from cloud builder

## 2. Target environment

Decide which environment you want to target, then enter a shell configured to target it.

```
./target-env <dev | prod>
```

## 3. Provision infrastructure

```
./terraform-init
terraform plan # Shows you what the next step will do
terraform apply
```

## 4. Configure cluster

Enable Cloud SQL Administration API - https://console.cloud.google.com/flows/enableapi?apiid=sqladmin&redirect=https://console.cloud.google.com&_ga=1.69095313.1440849751.1500257608

```
./init-db_access
./cluster-apply-cloudsqlproxy
./cluster-apply-taskman
./cluster-apply-webapp
```


Deploying Updates
=================
