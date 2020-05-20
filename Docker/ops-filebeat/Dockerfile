FROM docker.elastic.co/beats/filebeat-oss:7.6.2

# Because we're mounting external volumes
USER root

ENTRYPOINT ["/usr/share/filebeat/start"]
CMD ["-e", "-strict.perms=false"]

COPY ./start /usr/share/filebeat/start
COPY ./filebeat.yml /usr/share/filebeat/filebeat.yml
