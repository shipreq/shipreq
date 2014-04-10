OS & Environment Setup
======================

* export ip=$(../util/ip-shipreq)

* root/deploy $ip

* ssh root@$ip

    ./init-packages
    ./fix-yaourt-micro     # Only if running a micro instance
    ./init-packages2
    ./init-instance_store  # Unless running a micro instance
    reboot

