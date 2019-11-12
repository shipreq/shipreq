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

* `modules/shipreq-env` - Configurable Terraform *module* to create a ShipReq runtime environment.
                          Depends on `init` and `global`.

* `dev` - Terraform to setup a ShipReq dev environment.
          Depends on `init` and `global`.


Initial Setup
=============

1. Create an AWS account
2. Have a user account and local env setup ([see](../AWS.md))
3. Terraform: `init`
4. Terraform: `global`
5. Terraform: `cicd`
6. CodeBuild: `images`
7. CodeBuild: `shipreq`


Env Details
===========

* One VPC per env
  * 2 subnets: public, private
  * 2 private DNSs only accessible from within the VPC:
    * `<env>.internal` - manually managed
    * `<env>.sd.internal` - managed by service discovery
