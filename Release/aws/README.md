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


Remote Service Setup
====================

1. Select zone
  * AWS console: https://console.aws.amazon.com/ec2/v2/home
  * In top right: Sydney

2. Security groups
  * ./group-webapp-create
  * ./group-devaccess-redo

3. Create EC2 instance
  * AWS console > EC2
  * Launch Instance
    * Community AMIs
    * Go to https://www.uplinklabs.net/projects/arch-linux-on-ec2/
    * Select S3 paravirtual, copy ami-12345678
    * Back in Community AMIs, paste ami-12345678, press enter.
    * Choose instance type.
    * Keep hitting Next until get to `Configure Security Group`.
    * Select existing security group: webapp
    * Launch.
  * Connect
    * AWS console > Instances
    * ec2-get-console-output `instance_id`
    * Click instance and copy `Public DNS`
    * export ip=<public dns>
    * ssh root@$ip

4. Create database
  * AWS console > RDS
  * Launch a DB Instance
  * PostgreSQL
  * DB Instance Details:
    * Allocated Storage: 10 GB
    * DB Instance Identifier: shipreq-db
    * Username/password: <whatever>. Store it in KeePass.
  * Additional Config
    * Database Name: shipreq_db
    * Availability Zone: Same as EC2 instance.
  * Management Options
    * Backup Window: 18:00-18:30 (UTC)
    * Maintenance Window: Sat 19:00-19:30 (UTC)
  * RDS > Security Groups
    The following steps will allow access from EC2 instances in the webapp security group.
    * Default
    * Connection type: EC2
    * EC2 Security Name: webapp
    * Authorize

Notes
=====

### Storage

* Instance = local, transient.
* EBS = network, persistent, fast, snapshotable, bootable, seperate charges.
* S3  = network, persistent, slow.


### Virtualisation

* PV (Paravirtual)
  * Fast
  * Linux-only

* HVM (Hardware-assisted Virtual Machine)
  * Slow
  * Better hardware isolation
