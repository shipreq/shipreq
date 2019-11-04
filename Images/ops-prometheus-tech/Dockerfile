FROM prom/prometheus:v2.13.1

# Because /data will be mounted as root
USER root
VOLUME /data

ENTRYPOINT ["/root/start"]
CMD []
COPY ./start /root/start
COPY ./prometheus.yml /root/prometheus.yml.tempalte
