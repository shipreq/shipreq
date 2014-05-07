OS & Environment Setup
======================

* export ip=$(../util/ip-shipreq)

* root/deploy $ip

* ssh root@$ip

    ./init-packages
    ./fix-yaourt-micro     # Only if running a micro instance
    ./init-packages2

    # Unless running a micro instance
    lsblk
    echo /dev/xvda2 > instance_store-device
    ./instance_store-init
    ./instance_store-swap <size>

    reboot
    ./instance_store-reinit

