Prerequisites
=============

### User account

1. Create an IAM user with the Administrator group
2. Update your SSH key to that IAM user
3. Follow the CodeCommit SSH instructions to create a `~/.ssh/config`
4. Run `aws configure`, enter your two keys, set region to `ap-southeast-2` and output to `json`

### AWS account

1. Apply the terraform in `./init`.
   It will create the S3 bucket that all the other terraform will use to store its state.
