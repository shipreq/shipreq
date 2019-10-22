#!/bin/bash

set -euo pipefail

device=${_device}
tagValue=${_tagValue}
mount=${_mount}

instance_id="$(curl -s http://169.254.169.254/latest/meta-data/instance-id)"
az="$(curl -s http://169.254.169.254/latest/meta-data/placement/availability-zone)"

########################################################################################################################
# Mount volume

function descvols {
  aws ec2 describe-volumes --filters "Name=tag:ecs-ebs,Values=$tagValue" "$@" | sed 's/"//g'
}

while [ ! -e $device ]; do

  attach_state="$(descvols "Name=attachment.instance-id,Values=$instance_id" --query 'Volumes[0].Attachments[0].State')"
  if [ "$attach_state" != "null" ]; then
    echo "Volume still attaching..."
    sleep 2
  else

    echo "Looking for available $tagValue volume..."
    vol="$(descvols "Name=status,Values=available" --query 'Volumes[0].VolumeId')"
    echo "  $vol"
    if [ "$vol" = "null" ]; then
      sleep 4
    else
      echo "Attaching volume..."
      aws ec2 attach-volume --device $device --instance-id $instance_id --volume-id $vol
      sleep 2
    fi
  fi

done

echo "Volume mounted at $device"
echo

########################################################################################################################
# Format volume

if [[ "$(file -sL $device)" =~ .*:\ data$ ]]; then
  echo "Formatting $device..."
  mkfs -t ext4 $device
fi

echo "Volume is formatted"
file -sL $device
echo

########################################################################################################################
# Mount fs

if [ -z "$(mount | fgrep " $mount ")" ]; then
  echo "Mounting at $mount..."
  mkdir -p "$mount"
  mount -o defaults,noatime $device "$mount"
fi

echo "Volume is mounted at $mount"
df -h "$mount"
