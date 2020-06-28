FROM prom/node-exporter:v1.0.1

USER root

RUN mkdir -p /var/lib/dpkg/info /var/lib/dpkg/status \
 && wget http://ftp.au.debian.org/debian/pool/main/b/bash/bash-static_5.0-4_amd64.deb \
 && dpkg -i --force-depends bash-static_5.0-4_amd64.deb \
 && rm bash-static_5.0-4_amd64.deb \
 && mv /bin/bash-static /bin/bash

COPY ./docker-entrypoint.sh /docker-entrypoint.sh
ENTRYPOINT ["/docker-entrypoint.sh"]
CMD []

USER nobody
