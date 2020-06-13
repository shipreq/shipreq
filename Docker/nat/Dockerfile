FROM alpine:3.12.0

RUN apk --update --no-cache add \
      bash \
      ca-certificates \
      curl \
      openssl \
      squid \
      vim

EXPOSE 3128 3129 3130

RUN /usr/lib/squid/security_file_certgen -c -s /var/cache/squid/ssl_db -M 4MB

COPY ./ssl /etc/squid/ssl

COPY ./docker-entrypoint.sh /docker-entrypoint.sh
CMD ["/docker-entrypoint.sh"]

RUN mv /etc/squid/squid.conf /etc/squid/squid.conf.orig
COPY ./squid.conf /etc/squid/squid.conf

COPY ./domain-whitelist.txt /etc/squid/domain-whitelist.txt
