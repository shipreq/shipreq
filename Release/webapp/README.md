First-Time Procedure
====================

###  Local setup

* Install required packages:
    sudo pacman -Syu --needed parallel pigz authbind

* Decrypt passwords and sensitive settings.
    ./secrets-decrypt

* Install ShipReq WAR.
    Build locally first.
    ./install-war

* Determine IP for commands below
    export ip=$(../util/ip-shipreq)

### Deployment

    ./deploy-jetty $ip
    ./deploy-package $ip
    ./deploy-war $ip
    ssh $(<deployment-user)@$ip webapp/bin/restart


Upgrade Procedure
=================

### Upgrading ShipReq

    ./install-war
    ./deploy-package $ip   # If needed
    ./deploy-war $ip
    ssh $(<deployment-user)@$ip webapp/bin/restart


### Upgrading Jetty

1. Install
    ./install-jetty <jetty.tar.gz>
    git commit as directed

2. Customisation
    ./jetty-post_install
    git add -A .

3. Test Locally
    webapp/bin/start
    ../../../QA/smoke_test-frontend local-https

4. Update Code
    pushd ../../Code
    vim project/Dependencies.scala
    sbt clean test
    popd

5. Commit

6. Deploy
    ./deploy-jetty $ip
    ./deploy-package $ip   # If needed
    ssh $(<deployment-user)@$ip webapp/bin/restart


Keystore & SSL
==============

[Good reference](http://www.akadia.com/services/ssh_test_certificate.html)

### From Nada to a Certificate

* Generate key & CSR (Certificate Signing Request)
    openssl req -nodes -newkey rsa:2048 -keyout www.shipreq.com.key -out www.shipreq.com.csr
  When it asks for Common Name, enter www.shipreq.com

* Give CSR to CA. Alternatively, can self-sign:
    openssl x509 -req -days 365 -in www.shipreq.com.csr -signkey www.shipreq.com.key -out www.shipreq.com.crt

### From Certificate to a keystore

* Already available should be:
    www.shipreq.com.crt
    www.shipreq.com.csr
    www.shipreq.com.key

* Combine key and crt into a pkcs12.
    openssl pkcs12 -inkey www.shipreq.com.key -in www.shipreq.com.crt -export -out www.shipreq.com.pkcs12

* Create a keystore with both the certificate and the pkcs12.
    k=etc/keystore
    keytool -keystore $k -import -alias shipreq_cert -file www.shipreq.com.crt
    keytool -importkeystore -srckeystore www.shipreq.com.pkcs12 -srcstoretype PKCS12 -destkeystore $k
    keytool -changealias -alias 1 -destalias shipreq_key -keystore $k

### Jetty Integration

* Obfuscate passwords.
    ./jetty-password_hash PASS1
    ./jetty-password_hash PASS2

* Give Jetty the passwords.

  Jetty needs:
      KeyStorePassword   - Keystore password.
      TrustStorePassword - Keystore password.
      KeyManagerPassword - PKCS12 password.

  These are already configured to be provided via properties.
  The properties are in `start.d/ssl-passwords.ini` and look like this:
      jetty.keystore.password=OBF:blah1
      jetty.truststore.password=OBF:blah1
      jetty.keymanager.password=OBF:blah2

### Replacing the Certificate

Usually done when the certificate expires. Gandi provides a new .crt file.

1. `cd ~/BeardedLogic/cert` and replace the old crt with the new one.
2. Generate a `pkcs12` as shown above. Use a new password and put in KeePass.
3. Generate a keystore as shown above. Use a new password and put in KeePass.
4. Move keystore into `Release/webapp/webapp/etc`.
5. Follow `Jetty Integration` instructions.
6. Put shipreq.com in `/etc/hosts`, start locally, test.
7. Deploy and restart.
    ./deploy-package $ip
    ssh $(<deployment-user)@$ip webapp/bin/restart
8. Encrypt for Git: `./secrets-encrypt`
9. Commit.


Running, Usage, etc.
====================

See [webapp/README.md](webapp/README.md) for instructions.

