FROM grafana/grafana:6.4.4

USER root
RUN apk --no-cache add curl
USER grafana

COPY provisioning /etc/grafana/provisioning

ENV \
  GF_SERVER_ENABLE_GZIP=true \
  GF_SERVER_DOMAIN=grafana.whatever.com \
  GF_SERVER_ROOT_URL=%(protocol)s://%(domain)s/grafana/ \
  GF_SERVER_SERVE_FROM_SUB_PATH=true
