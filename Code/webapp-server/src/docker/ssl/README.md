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
    GandiStandardSSLCA2.pem (Intermediate cert - login to gandi.net to download)

* Combine SSL & intermediate SSL into a cert chain
    cat www.shipreq.com.crt GandiStandardSSLCA2.pem > shipreq-gandi.crt

* Combine key and crt into a pkcs12.
    openssl pkcs12 -inkey www.shipreq.com.key -in shipreq-gandi.crt -export -out shipreq-gandi.pkcs12

* Create a keystore with both the certificate and the pkcs12.
    k=keystore
    keytool -keystore $k -import -alias shipreq_cert -file shipreq-gandi.crt -noprompt
    keytool -importkeystore -srckeystore shipreq-gandi.pkcs12 -srcstoretype PKCS12 -destkeystore $k
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
  The properties are in `ssl-passwords.ini` and look like this:
      jetty.sslContext.keyStorePassword=OBF:blah1
      jetty.sslContext.trustStorePassword=OBF:blah1
      jetty.sslContext.keyManagerPassword=OBF:blah2


### Replacing the Certificate

Usually done when the certificate expires. Gandi provides a new .crt file.

1. `cd ~/BeardedLogic/cert` and replace the old crt with the new one.
2. Generate a `pkcs12` as shown above. Use a new password and put in KeePass.
3. Generate a keystore as shown above. Use a new password and put in KeePass.
4. Replace keystore in `Code/webapp-server/src/docker/ssl`.
5. Put shipreq.com in `/etc/hosts`, start locally, test.

