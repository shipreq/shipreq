Overview
========

* `init` - Terraform to setup an S3 bucket for all other Terraform to store its state.

* `shared` - Shared Terraform code/vars.
             Used by symlinking from other directories.

* `global` - Terraform to setup environment-agnostic stuff.
             (eg. docker repos, proof of shipreq.com for 3rd party services)


Prerequisites
=============

1. Have a user account and local env setup (see ../AWS.md)
2. Apply the terraform in `./init`.
