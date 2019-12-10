Overview
========

* `init` - Terraform to setup an S3 bucket for all other Terraform to store its state.

* `global` - Terraform to setup environment-agnostic stuff.
             (eg. docker repos, proof of shipreq.com for 3rd party services)
             Depends on `init`.

* `cicd` - Terraform to setup CI/CD stuff.
           Depends on `init` and `global`.

* `modules/ec2-sd` - Terraform *module* for EC2 service discovery.
                     Specifically, the ability to have a single, private DNS record that points all
                     live EC2s with a given Name tag.

* `modules/ecs-ebs` - Terraform *module* to allow each EC2 in a cluster, a persistent EBS volume.

* `modules/ecs-monitoring` - Terraform *module* to add a bunch of daemon monitoring containers to an ECS cluster.

* `modules/shipreq-env` - Configurable Terraform *module* to create a ShipReq runtime environment.
                          Depends on `init` and `global`.

* `dev` - Terraform to setup a ShipReq dev environment.
          Depends on `init` and `global`.


Initial Setup
=============

1. Create an AWS account
2. Have a user account and local env setup ([see](../AWS.md))
3. Apply Terraform in `init`
4. Apply Terraform in `global`
5. Apply Terraform in `cicd`
6. Build docker images - `aws codebuild start-build --project-name images`
7. Build ShipReq       - `aws codebuild start-build --project-name shipreq`


Defining a New Environment
==========================

1. Make a directory for your new environment, and enter it. (eg. `mkdir qa; cd qa`)
2. Generate SSH keys - `../gen-keys`
3. Create property files `webapp.properties` and `taskman.properties`
4. Copy the Terraform from another env and modify it. (eg. `cp ../dev/dev.tf qa.tf`)


Deploying a New Environment
===========================

Ideally this would just be a simple `terraform apply` but unfortunately it's not.

1. Setup infrastructure
  1. `terraform init`
  2. `terraform apply`

2. Initialise DB
  1. Configure local machine for bastion access (instructions below)
  2. `ssh shipreq-bastion-${env}`
  3. `postgres root` and enter root password from `${env}.tf`
  4. Locally run `../shipreq-db-setup-sql.sh` and paste the output into `psql`


Configure local machine for bastion access
==========================================

1. Run `./install-ssh`
2. If the environment is prod, edit `~/.ssh/config` and
   replace `b.prod.shipwreck.space` with the real Bastion address or IP that Terraform spits out.


Viewing the Ops Portal
======================

1. Run `ssh -NL 10000:localhost:8000 shipreq-bastion-${env}` to create a tunnel.
2. Open http://localhost:10000/
