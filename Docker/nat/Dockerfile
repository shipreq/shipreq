# See https://github.com/strowi/docker-alpine-squid/blob/master/Dockerfile

FROM alpine:3.12.0

RUN apk --update --no-cache add acf-squid openssl \
  && rm -fr /var/cache/apk/* \
    /tmp/* \
    /root/.cache \
    /root/.cached

COPY ./docker-entrypoint.sh /docker-entrypoint.sh
CMD ["/docker-entrypoint.sh"]

COPY ./squid.conf /etc/squid/squid.conf

EXPOSE 3128 3129 3130

COPY ./domain-whitelist.txt /etc/squid/domain-whitelist.txt

RUN mkdir /etc/squid/ssl \
 && cd /etc/squid/ssl \
 && openssl genrsa -out squid.key 4096 \
 && openssl req -new -key squid.key -out squid.csr -subj "/C=XX/ST=XX/L=squid/O=squid/CN=squid" \
 && openssl x509 -req -days 3650 -in squid.csr -signkey squid.key -out squid.crt \
 && cat squid.key squid.crt >> squid.pem \
 && chmod 400 squid.*
