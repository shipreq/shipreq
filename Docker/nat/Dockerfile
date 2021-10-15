# FROM alpine:3.12.4
FROM public.ecr.aws/micahhausler/alpine:3.14.0

RUN apk --update --no-cache add \
      bash \
      ca-certificates \
      curl \
      openssl \
      squid \
      vim

EXPOSE 3128 3129 3130

COPY ./ssl /etc/squid/ssl
RUN chown -R squid:squid /etc/squid/ssl

COPY ./docker-entrypoint.sh /docker-entrypoint.sh
CMD ["/docker-entrypoint.sh"]

COPY ./healthcheck /healthcheck
HEALTHCHECK CMD /healthcheck

COPY ./squid.conf           /etc/squid/squid.conf
COPY ./domain-whitelist.txt /etc/squid/domain-whitelist.txt
