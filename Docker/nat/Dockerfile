# See https://github.com/strowi/docker-alpine-squid/blob/master/Dockerfile

FROM alpine:3.12.0

RUN apk --update --no-cache add \
      bash \
      ca-certificates \
      openssl \
      squid \
      vim

EXPOSE 3128 3129 3130

RUN mkdir /etc/squid/ssl \
 && cd /etc/squid/ssl \
 && openssl genrsa -out squid.key 4096 \
 && openssl req -new -key squid.key -out squid.csr -subj "/C=XX/ST=XX/L=squid/O=squid/CN=squid" \
 && openssl x509 -req -days 3650 -in squid.csr -signkey squid.key -out squid.crt \
 && cat squid.key squid.crt >> squid.pem \
 && chmod 400 squid.*

COPY ./docker-entrypoint.sh /docker-entrypoint.sh
CMD ["/docker-entrypoint.sh"]

RUN mv /etc/squid/squid.conf /etc/squid/squid.conf.orig
COPY ./squid.conf /etc/squid/squid.conf

COPY ./domain-whitelist.txt /etc/squid/domain-whitelist.txt
