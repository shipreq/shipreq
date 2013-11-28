Getting Started
===============

* Decrypt the SSL passwords.
    cd start.d && ./decrypt

* Install webapps/shipreq.war


Usage
=====

* Run
    ./jetty

* Start daemon
    ./jettyd start

* Stop daemon
    ./jettyd stop


Config
======

* Inspection
    ./jetty --list-config
    ./jetty --list-modules
    ./jettyd check

* Jetty config:
    * start.ini
    * start.d/*.ini
    * etc/jetty.conf [daemon-mode only]
    * XMLs displayed in `./jetty --list-config`

* App config:
    * resources/*
    * start.d/shipreq.ini


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
    keytool -keystore etc/keystore -import -alias shipreq_cert -file www.shipreq.com.crt
    keytool -importkeystore -srckeystore www.shipreq.com.pkcs12 -srcstoretype PKCS12 -destkeystore etc/keystore
    keytool -changealias -alias 1 -destalias shipreq_key -keystore etc/keystore

### Jetty Integration

* Obfuscate passwords.
    java -cp jetty-inst/lib/jetty-util-9.1.0.v20131115.jar org.eclipse.jetty.util.security.Password PASS1
    java -cp jetty-inst/lib/jetty-util-9.1.0.v20131115.jar org.eclipse.jetty.util.security.Password PASS2

* Give Jetty the passwords.
    KeyStorePassword   - Keystore password.
    TrustStorePassword - Keystore password.
    KeyManagerPassword - PKCS12 password.

