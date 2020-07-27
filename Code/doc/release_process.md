Release Process
===============

1. `git push aws master`
2. `aws codebuild start-build --project-name shipreq`
3. Wait for AWS CodeBuild to finish building
4. `cd Terraform/prod`
5. `git rev-parse HEAD` and copy the output
6. Edit `versions.tf`: modify the `shipreq` line to use the above git sha
7. `terraform apply`
8. `git add versions.tf && git commit -m 'Prod: Upgrade ShipReq' versions.tf`
