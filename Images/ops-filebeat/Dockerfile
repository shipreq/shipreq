FROM docker.elastic.co/beats/filebeat-oss:7.4.2

# Because we're mounting external volumes
USER root

COPY ./filebeat.yml /usr/share/filebeat/filebeat.yml

CMD ["-e", "-strict.perms=false"]