Local Machine Setup
===================

* Get access keys from Keepass or create new ones
  * https://console.aws.amazon.com/iam/home?#security_credential
  * Access Keys
* localrc
    export AWS_ACCESS_KEY=
    export AWS_SECRET_KEY=

* To set the default region, get the URL for it:
    ec2-describe-regions
* localrc
    export EC2_URL=https://<service_endpoint>


Create a New EC2 Instance
=========================

* AWS console. https://console.aws.amazon.com/ec2/v2/home
* EC2
* In top right: Sydney
* Launch Instance
  * Community AMIs
  * Go to https://www.uplinklabs.net/projects/arch-linux-on-ec2/
  * Select S3 paravirtual, copy ami-12345678
  * Back in Community AMIs, paste ami-12345678, press enter.
  * Select and follow wizard.
* ec2-get-console-output instance_id

* AWS console > Instances > Public DNS

