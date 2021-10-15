Release Process
===============


### Part 1: Building

* Remotely
  1. `git push aws master`
  2. `aws codebuild start-build --project-name shipreq`
  3. Wait for AWS CodeBuild to finish building

* Locally
  1. `make ci-local`


### Part 2: Deployment
1. `cd ../Terraform/prod`
2. `git rev-parse HEAD` and copy the output
3. Edit `versions.tf`: modify the `shipreq` line to use the above git sha
4. `terraform apply`
5. `git add versions.tf && git commit -m 'Prod: Upgrade ShipReq' versions.tf`
