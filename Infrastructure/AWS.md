Manual Steps
============

### Add MFA to root account

1. Click *ShipReq* in top-right
2. Click *My Security Credentials*
3. Install *Google Authenticator* on phone
4. Activate *virtual MFA device*
5. Follow instructions

### User accounts

It's a good practice to create an admin user account and use that rather than root.

I manually created one before deciding I need much more detail in these docs (like I'm writing now)
so the exact steps have been lost. This seems to be what I did though:

* created an IAM Group called "Admin" with the "AdministratorAccess" policy
* created an IAM User called "golly" with the "Admin" group


User / Local Env Setup
======================

1. Login to AWS as non-root IAM user and
  1. activate MFA
  2. create an access key
2. run `aws configure`
  1. enter account keys
  2. set region to `ap-southeast-2`
  3. set output to `json`
