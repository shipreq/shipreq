FROM postgres:13.5

RUN apt-get -qq update \
 && apt-get -qq -y --no-install-recommends install \
      postgresql-13-hll

COPY ./create-extensions.sql /docker-entrypoint-initdb.d/00-create-extensions.sql

RUN chmod 666 /docker-entrypoint-initdb.d/00-create-extensions.sql
